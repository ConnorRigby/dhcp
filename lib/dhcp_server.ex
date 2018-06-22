defmodule DHCPServer do
  @moduledoc """
  Manages DHCP connections.

  If NervesNetwork exists, it will manage bringing up and tearing down of
  the interface.
  """

  use Supervisor
  use Bitwise

  @default [
    authoritative: true,
    lease_time: 3600,
    gateway: "192.168.24.1",
    netmask: "255.255.255.0",
    range: {"192.168.24.2", "192.168.24.100"},
    domain_servers: ["192.168.24.1"],
  ]

  @doc """
  Start a dhcp server.
  # interface
    * A linux network interface name.
  # config
    * `authoritative`  - default: `#{@default[:authoritative]}`.
    * `lease_file`     - default: `"/var/run/dhcp_leases_<interface>.dets"`.
    * `lease_time`     - default: `#{@default[:lease_time]}`.
    * `gateway`        - default: `#{@default[:gateway]}`.
    * `netmask`        - default: `#{@default[:netmask]}`.
    * `range`          - default: `#{inspect @default[:range]}`.
    * `domain_servers` - default: `#{inspect @default[:domain_servers]}`.
    * `domain_name`    - default: `"node()"`.
  """
  def start_link(interface, config) do
    Supervisor.start_link(__MODULE__, [interface, config], [name: :"#{__MODULE__}-#{interface}"])
  end

  @doc "stop a dhcp server."
  def stop(interface) do
    Supervisor.stop(:"#{__MODULE__}-#{interface}")
  end

  ## Supervisor

  def init([interface, config]) do
    opts = config |> parse_config(interface)
    {net_name_space, interface, server_id, next_server, lease_file, subnets, hosts} = :dhcp_server_config.parse_config(opts)

    children = [
      worker(DHCPServer.Server,  [net_name_space, interface, server_id, next_server]),
      worker(:dhcp_server_alloc, [interface, lease_file, subnets, hosts]),
      worker(DHCPServer.Worker,  [opts])
    ]
    supervise(children, [strategy: :one_for_one])
  end

  @doc false
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, opts},
      type: :supervisor
    }
  end

  ## Private

  defp parse_config(opts, interface) do
    # Raise an argument error here if not supplied.
    authoritative  = get_config(opts, :authoritative)
    lease_file     = Keyword.get(opts, :lease_file, '/var/run/dhcp_leases_#{interface}.dets')
    lease_time     = get_config(opts, :lease_time)

    gateway        = get_config(opts, :gateway)
    netmask        = get_config(opts, :netmask)
    {begin, fin}   = get_config(opts, :range)
    domain_servers = get_config(opts, :domain_servers)

    domain_name    = Keyword.get(opts, :domain_name, default_domain()) |> to_charlist

    # Convert binary values to IP address tuples.
    gateway = ip(gateway)
    netmask = ip(netmask)
    begin = ip(begin)
    fin = ip(fin)
    domain_servers = Enum.map(domain_servers, &ip/1)

    network = get_network(gateway, netmask)
    broadcast_addr = get_broadcast_addr(network, netmask)
    validate_range!(begin, fin, network, netmask)

    # This is the format that the server expects.
    # I don't know what the random numbers are.
    [
      {:server_id, gateway},
      {:interface, interface},
      {:authoritative, authoritative},
      {:lease_file, lease_file},
      {:netns, nil},

      {:subnet,
       network,                             #  Network
       netmask,                             #  Netmask
       {begin,fin},                         # Range
       [
        {1,  netmask                  },    #  Subnet Mask
        {28, broadcast_addr           },    #  Broadcast Address
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
    addr_bin
    |> String.to_charlist()
    |> :inet.parse_ipv4_address()
    |> case do
      {:ok, ip_addr} -> ip_addr
      _ -> raise ArgumentError, "#{addr_bin} is not a valid ip address!"
    end
  end

  # Gets the network for an IP address given its netmask.
  defp get_network(ip_addr, netmask)
  defp get_network({a, b, c, d}, {m_a, m_b, m_c, m_d}) do
    {a &&& m_a, b &&& m_b, c &&& m_c, d &&& m_d}
  end

  # Gets the broadcast address for a network.
  defp get_broadcast_addr(ip_addr, netmask)
  defp get_broadcast_addr({a, b, c, d}, {m_a, m_b, m_c, m_d}) do
    # Add 256 after a binary NOT as values are not limited to a byte.
    # (~~~0x(...000000)FF becomes 0x(...111111)00, so adding 256 put it back in
    # the range)
    {a ||| (~~~m_a + 256), b ||| (~~~m_b + 256), c ||| (~~~m_c + 256), d ||| (~~~m_d + 256)}
  end

  defp validate_range!(begin, fin, network, netmask) do
    begin_net = get_network(begin, netmask)
    fin_net = get_network(fin, netmask)

    unless begin_net === network && fin_net === network do
      raise ArgumentError, "The DHCP range is not in the same network as the gateway"
    end
  end

  defp default_domain do
    default = node() |> to_string()
    case String.split(default, "@") do
      [_, domain] -> "#{domain}.local"
      [domain] -> "#{domain}.local"
    end
  end

end
