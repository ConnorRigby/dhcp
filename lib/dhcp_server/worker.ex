defmodule  DHCPServer.Worker do
  @moduledoc  "Handles bringing an interface up and down."

  use GenServer
  require Logger
  
  @doc false
  def start_link(config) do
    GenServer.start_link(__MODULE__, config)
  end

  def init(config) do
    interface = :proplists.get_value(:interface, config)
    # im so sorry.
    {:subnet, gateway, netmask, _range, [_, _, _, {_, domain}, {_, name_servers}, _]} = Enum.find(config, fn(f) -> match?({:subnet, _, _, _, _}, f) end)
    Nerves.Network.setup(interface, [ipv4_address_method: :static, ipv4_address: ip_bin(gateway), ipv4_subnet_mask: ip_bin(netmask), domain: to_string(domain), nameservers: ip_bin(name_servers)])
    Logger.debug "DHCPServer setting up #{interface}"
    {:ok, interface}
  end

  defp ip_bin(list) when is_list(list) do
    Enum.map(list, &ip_bin(&1))
  end

  defp ip_bin({a, b, c, d}) do
    "#{a}.#{b}.#{c}.#{d}"
  end

  def terminate(_, interface) do
    Logger.debug "DHCPServer tearing down #{interface}"
    Nerves.Network.teardown(interface)
  end

end
