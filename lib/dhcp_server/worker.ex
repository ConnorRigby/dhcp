defmodule  DHCPServer.Worker do
  @moduledoc  "Handles bringing an interface up and down."

  @interface_handler    __MODULE__
  if Code.ensure_loaded?(Nerves.Network) do
    @interface_handler Nerves.Network
  else
    IO.warn """
    DHCPServer will not setup or tear down network interfaces.
    If you would like it to, add:

          {:nerves_network, ">= 0.0.0"}

    to you're mix.exs file.
    """, []

    @doc false
    def setup(_interface, _opts), do: :ok
    @doc false
    def teardown(_interface), do: :ok
  end

  use GenServer
  require Logger

  @doc false
  def start_link(config) do
    GenServer.start_link(__MODULE__, config)
  end

  def init(config) do
    interface = :proplists.get_value(:interface, config)
    # im so sorry.
    {:subnet, _, netmask, _range, [_, _, {_, [gateway]}, {_, domain}, {_, name_servers}, _]} = Enum.find(config, fn(f) -> match?({:subnet, _, _, _, _}, f) end)
    @interface_handler.setup(interface, [ipv4_address_method: :static, ipv4_address: ip_bin(gateway), ipv4_subnet_mask: ip_bin(netmask), domain: to_string(domain), nameservers: ip_bin(name_servers)])
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
    @interface_handler.teardown(interface)
  end
end
