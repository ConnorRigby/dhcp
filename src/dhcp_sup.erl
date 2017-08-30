%%%-------------------------------------------------------------------
%%% File    : dhcp_sup.erl
%%% Author  : Ruslan Babayev <ruslan@babayev.com>
%%% Description :
%%%
%%% Created : 20 Sep 2006 by Ruslan Babayev <ruslan@babayev.com>
%%%-------------------------------------------------------------------
-module(dhcp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([get_config/0]).
%% Supervisor callbacks
-export([init/1]).

-import(lists, [keysearch/3, filter/2]).

-include("dhcp_alloc.hrl").

-define(SERVER, ?MODULE).
-define(DHCP_LEASEFILE, "/var/run/dhcp_leases.dets").

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    lager:info("Starting supervisor."),
    case get_config() of
        {ok, NetNameSpace, Interface, ServerId, NextServer, LeaseFile, Subnets, Hosts} ->
            DHCPServer = {dhcp_server, {dhcp_server, start_link,
                                        [NetNameSpace, Interface, ServerId, NextServer]},
                          permanent, 2000, worker, [dhcp_server]},
            DHCPAlloc = {dhcp_alloc, {dhcp_alloc, start_link,
                                      [LeaseFile, Subnets, Hosts]},
                         permanent, 2000, worker, [dhcp_alloc]},
            {ok, {{one_for_one, 0, 1}, [DHCPServer, DHCPAlloc]}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
get_config() ->
  Env = 'Elixir.Application':get_all_env(dhcp),
  'Elixir.IO':inspect(Env),
  % {ok, NetNameSpace, Interface, ServerId, NextServer, LeaseFile, Subnets, Hosts};
  {error, no_conf}.
