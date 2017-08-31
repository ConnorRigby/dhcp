defmodule DHCPServer do
  @moduledoc """
  Manages DHCP connections.

  If NervesNetwork exists, it will manage bringing up and tearing down of
  the interface.
  """

  use Supervisor

  @default [
    authoritative: true,
    lease_file: "/var/run/dhcp_leases.dets",
    lease_time: 3600,
    gateway: "192.168.24.1",
    netmask: "255.255.255.0",
    range: {"192.168.24.2", "192.168.24.100"},
    domain_servers: ["192.168.24.1"]
  ]

  @doc """
  Start a dhcp server.
  # Config
    * `interface`     - (required) A network interface. IE: `wlan0`.
    * `authoritative` - default: `#{@default[:authoritative]}`.
    * `lease_file`    - default: `#{@default[:lease_file]}`.
    * `lease_time`    - default: `#{@default[:lease_time]}`.
    * `gateway`       - default: `#{@default[:gateway]}`.
    * `netmask`       - default: `#{@default[:netmask]}`.
    * `range`          - default: `#{inspect @default[:range]}`.
    * `domain_servers` - default: `#{inspect @default[:domain_servers]}`.
    * `domain_name`    - default: `"node()"`.
  """
  def start_link(config, opts \\ []) do
    Supervisor.start_link(__MODULE__, config, opts)
  end

  @doc "stop a dhcp server."
  def stop(dhcp_server) do
    Supervisor.stop(dhcp_server)
  end

  ## Supervisor

  def init(config) do
    opts = config |> parse_config()
    {net_name_space, interface, server_id, next_server, lease_file, subnets, hosts} = :dhcp_server_config.parse_config(opts)

    children = [
      worker(:dhcp_server,       [net_name_space, interface, server_id, next_server], [name: :"dhcp_server_#{interface}"]),
      worker(:dhcp_server_alloc, [lease_file, subnets, hosts], [name: :"dhcp_server_alloc_#{interface}"]),
      worker(DHCPServer.Worker, [opts], [name: :"dhcp_worker_#{interface}"])
    ]
    supervise(children, [strategy: :one_for_one])
  end

  ## Private

  defp parse_config(opts) do
    # Raise an argument error here if not supplied.
    interface      = get_config(opts, :interface) |> to_string
    authoritative  = get_config(opts, :authoritative)
    lease_file     = get_config(opts, :lease_file) |> to_charlist
    lease_time     = get_config(opts, :lease_time)

    gateway        = get_config(opts, :gateway)
    netmask        = get_config(opts, :netmask)
    {begin, fin}   = get_config(opts, :range)
    domain_servers = get_config(opts, :domain_servers)

    domain_name    = Keyword.get(opts, :domain_name, default_domain()) |> to_charlist

    {a, b, c, _} = gateway = ip(gateway)
    netmask        = ip(netmask)
    base = {a, b, c}
    begin          = validate_ip(begin, base)
    fin            = validate_ip(fin, base)
    domain_servers = validate_ip(domain_servers, base)

    # This is the format that the server expects.
    # I don't know what the random numbers are.
    [
      {:server_id, gateway},
      {:interface, interface},
      {:authoritative, authoritative},
      {:lease_file, lease_file},
      {:netns, nil},

      {:subnet,
       {a, b, c, 0},                        #  Network
       netmask,                             #  Netmask
       {begin,fin},                         # Range
       [
        {1,  netmask                  },    #  Subnet Mask
        {28, {a, b, c, 255}           },    #  Broadcast Address
        {3,  [gateway]                },    #  Routers
        {15, domain_name              },    #  Domain Name
        {6,  domain_servers           },    #  Domain Name Servers
        {51, lease_time               }     #  Address Lease Time
       ]
      }
    ]
  end

  defp get_config(opts, key) do
    Keyword.get(opts, key, @default[key]) || raise ArgumentError, "Unsupplied key: #{key}"
  end

  defp ip(addr_bin) do
    case String.split(addr_bin, ".") do
      [a, b, c, d] ->  {String.to_integer(a), String.to_integer(b), String.to_integer(c), String.to_integer(d)}
      _ -> raise ArgumentError, "#{addr_bin} is not a valid ip address!"
    end
  end

  defp validate_ip(ip_list, base) when is_list(ip_list) do
    Enum.map(ip_list, &validate_ip(&1, base))
  end

  defp validate_ip(addr_bin, {base_a, base_b, base_c} = base) when is_binary(addr_bin) do
    {addr_a, addr_b, addr_c, _addr_d} = ip_addr = ip(addr_bin)
    if {addr_a, addr_b, addr_c} == base do
      ip_addr
    else
      raise ArgumentError, "#{addr_bin} does not match base #{base_a}.#{base_b}.#{base_c}.x"
    end
  end

  defp default_domain do
    default = node() |> to_string()
    case String.split(default, "@") do
      [_, domain] -> domain
      [domain] -> domain
    end
  end

end
