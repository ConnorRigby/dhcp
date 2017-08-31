# DHCPServer

## Installation

The package can be installed by adding `dhcp_server` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:dhcp_server, "~> 0.1.0"}]
end
```

## Usage

```elixir
iex(1)> {:ok, dhcp_server} = DHCPServer.start_link(interface: "usb0")
{:ok, dhcp_server}
# Plug device in or something?
# Get ip address?
# Do some local work?
# Profit?
iex(2)> DHCPServer.stop(dhcp_server)
:ok
```

## TODO
This currently requires a dependency
[nerves_network](https://github.com/nerves-project/nerves_network/).
This is to bring an interface up, and then configure it with an ip address.
This dependency can be optional, to allow a user to configure their interface and
settings manually from linux.
