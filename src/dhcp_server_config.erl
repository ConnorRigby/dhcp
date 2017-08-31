-module(dhcp_server_config).
%%TODO Learn what that weird syntax is, and move this to elixir.
-export([parse_config/1]).

-define(DHCP_LEASEFILE, "/var/run/dhcp_leases.dets").
-include("dhcp_server_alloc.hrl").

parse_config(Terms) ->
  NetNameSpace = proplists:get_value(netns,       Terms),
  Interface =    proplists:get_value(interface,   Terms),
  ServerId =     proplists:get_value(server_id,   Terms, {0, 0, 0, 0}),
  NextServer =   proplists:get_value(next_server, Terms, {0, 0, 0, 0}),
  LeaseFile =    proplists:get_value(lease_file,  Terms, ?DHCP_LEASEFILE),
  Subnets =      [X || X <- Terms, is_record(X, subnet)],
  Hosts =        [X || X <- Terms, is_record(X, host)],
  {NetNameSpace, Interface, ServerId, NextServer, LeaseFile, Subnets, Hosts}.
