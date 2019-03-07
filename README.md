[![CircleCI](https://circleci.com/gh/FarmBot-Labs/dhcp_server.svg?style=svg)](https://circleci.com/gh/FarmBot-Labs/dhcp_server)
[![Hex version](https://img.shields.io/hexpm/v/dhcp_server.svg "Hex version")](https://hex.pm/packages/dhcp_server)
# DHCPServer

## Installation

The package can be installed by adding `dhcp_server` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:dhcp_server, "~> 0.7"}]
end
```

## Usage

### Start and Stop a  one off server
```elixir
iex(1)> {:ok, dhcp_server} = DHCPServer.start_link("usb0", [])
{:ok, dhcp_server}
# Plug device in or something?
# Get ip address?
# Do some local work?
# Profit?
iex(2)> DHCPServer.stop(dhcp_server)
:ok
```

### As part of an application startup
```elixir
def start(_type, _args) do
  dhcp_options = [
    gateway: "192.168.254.1",
    netmask: "255.255.255.0",
    range: {"192.168.254.10", "192.168.254.99"},
    domain_servers: ["192.168.254.1"]
  ]

  children = [
    {DHCPServer, ["eth0", dhcp_options]}
  ]

  opts = [strategy: :one_for_one, name: MyApp.Supervisor]
  Supervisor.start_link(children, opts)
end
```

## License
This project is originally located [here](https://github.com/tonyrog/dhcp) and
is licensed under GPLv2. See [COPYING](COPYING) for more details.
