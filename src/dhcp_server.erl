%%%-------------------------------------------------------------------
%%% File    : dhcp_server.erl
%%% Author  : Ruslan Babayev <ruslan@babayev.com>
%%% Description : DHCP server
%%%
%%% Created : 20 Sep 2006 by Ruslan Babayev <ruslan@babayev.com>
%%%-------------------------------------------------------------------
-module(dhcp_server).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("dhcp_server.hrl").

-on_load(init/0).

-define(SERVER, ?MODULE).
-define(DHCP_SERVER_PORT, 67).
-define(DHCP_CLIENT_PORT, 68).
-define(INADDR_ANY, {0, 0, 0, 0}).
-define(INADDR_BROADCAST, {255, 255, 255, 255}).

-record(state, {if_name, socket, server_id, next_server}).

-define(is_broadcast(D), (is_record(D, dhcp_server) andalso (D#dhcp_server.flags bsr 15) == 1)).

init() ->
    LibDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]),

    %% load our nif library
    case erlang:load_nif(filename:join(LibDir, "dhcp_server"), 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, Error} ->
            error_logger:error_msg("could not load dhcp_server nif library: ~p", [Error]),
            error({load_nif, Error})
    end.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(NetNameSpace, Interface, ServerId, NextServer) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  [NetNameSpace, Interface, ServerId, NextServer], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([NetNameSpace, Interface, ServerId, NextServer]) ->
    Options = get_sockopt(NetNameSpace, Interface),
    io:format("Opts: ~p~n", [Options]),
    case gen_udp:open(?DHCP_SERVER_PORT, Options) of
	{ok, Socket} ->
	    'Elixir.DHCPServer.Logger':debug("Starting DHCP server..."),
	    {ok, #state{if_name = Interface,
			socket = Socket,
			server_id = ServerId,
			next_server = NextServer}};
	{error, Reason} ->
	    'Elixir.DHCPServer.Logger':debug(io_lib:format("Cannot open udp port ~w",
				   [?DHCP_SERVER_PORT])),
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({udp, Socket, IP, Port, Packet}, State = #state{socket = Socket}) ->
    Source = {IP, Port},
    Request = dhcp_server_lib:decode(Packet),
    case optsearch(?DHO_DHCP_MESSAGE_TYPE, Request) of
	{value, MsgType} ->
	    case handle_dhcp_server(MsgType, Request, State) of
		ok ->
		    ok;
		{reply, Reply} ->
		    send_reply(Source, MsgType, Reply, State);
		{error, Reason} ->
		    'Elixir.DHCPServer.Logger':debug(Reason);
		Other ->
		    'Elixir.DHCPServer.Logger':debug(io_lib:format("DHCP result: ~w", [Other]))
	    end;
	false ->
	    ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% The DHCP message handler
%%%-------------------------------------------------------------------
handle_dhcp_server(?DHCPDISCOVER, D, State) ->
    'Elixir.DHCPServer.Logger':debug(io_lib:format("DHCPDISCOVER from ~s ~s ~s",
			  [fmt_clientid(D), fmt_hostname(D), fmt_gateway(D)])),
    ClientId = get_client_id(D),
    Gateway = D#dhcp_server.giaddr,
    RequestedIP = get_requested_ip(D),
    case dhcp_server_alloc:reserve(ClientId, Gateway, RequestedIP) of
	{ok, IP, Options} ->
	    offer(D, IP, Options, State);
	Other ->
	    Other
    end;
handle_dhcp_server(?DHCPREQUEST, D, State) ->
    ClientId = get_client_id(D),
    'Elixir.DHCPServer.Logger':debug(io_lib:format("DHCPREQUEST from ~s ~s ~s",
			  [fmt_clientid(D), fmt_hostname(D), fmt_gateway(D)])),
    case client_state(D) of
	{selecting, ServerId} ->
	    case {ServerId, State#state.server_id} of
		{X, X} ->
		    IP = get_requested_ip(D),
		    case dhcp_server_alloc:allocate(ClientId, IP) of
			{ok, IP, Options} ->
			    ack(D, IP, Options, State);
			Other ->
			    Other
		    end;
		_ ->
		    %% Client selected someone else, do nothing
		    ok
	    end;
	{init_reboot, RequestedIP} ->
	    Gateway = D#dhcp_server.giaddr,
	    case dhcp_server_alloc:verify(ClientId, Gateway, RequestedIP) of
		{ok, IP, Options} ->
		    ack(D, IP, Options, State);
		noclient ->
		    'Elixir.DHCPServer.Logger':debug(io_lib:format("Client ~s has no current bindings",
					   [fmt_clientid(D)])),
		    ok;
		{error, Reason} ->
		    nak(D, Reason, State)
	    end;
	{ClientIs, IP} when ClientIs == renewing; ClientIs == rebinding ->
	    case dhcp_server_alloc:extend(ClientId, IP) of
		{ok, IP, Options} ->
		    ack(D, IP, Options, State);
		{error, Reason} ->
		    nak(D, Reason, State)
	    end
    end;
handle_dhcp_server(?DHCPDECLINE, D, _State) ->
    IP = get_requested_ip(D),
    'Elixir.DHCPServer.Logger':debug(io_lib:format("DHCPDECLINE of ~s from ~s ~s",
			  [fmt_ip(IP), fmt_clientid(D), fmt_hostname(D)])),
    dhcp_server_alloc:decline(IP);
handle_dhcp_server(?DHCPRELEASE, D, _State) ->
    ClientId = get_client_id(D),
    'Elixir.DHCPServer.Logger':debug(io_lib:format("DHCPRELEASE of ~s from ~s ~s ~s",
			  [fmt_ip(D#dhcp_server.ciaddr), fmt_clientid(D),
			   fmt_hostname(D), fmt_gateway(D)])),
    dhcp_server_alloc:release(ClientId, D#dhcp_server.ciaddr);
handle_dhcp_server(?DHCPINFORM, D, State) ->
    Gateway = D#dhcp_server.giaddr,
    IP = D#dhcp_server.ciaddr,
    'Elixir.DHCPServer.Logger':debug(io_lib:format("DHCPINFORM from ~s", [fmt_ip(IP)])),
    case dhcp_server_alloc:local_conf(Gateway) of
	{ok, Opts} ->
	    %% No Lease Time (RFC2131 sec. 4.3.5)
	    OptsSansLease = lists:keydelete(?DHO_DHCP_LEASE_TIME, 1, Opts),
	    ack(D, IP, OptsSansLease, State);
	Other ->
	    Other
    end;
handle_dhcp_server(MsgType, _D, _State) ->
    'Elixir.DHCPServer.Logger':debug(io_lib:format("Invalid DHCP message type ~p", [MsgType])),
    ok.

client_state(D) when is_record(D, dhcp_server) ->
    case optsearch(?DHO_DHCP_SERVER_IDENTIFIER, D) of
	{value, ServerId} ->
	    {selecting, ServerId};
	false ->
	    case optsearch(?DHO_DHCP_REQUESTED_ADDRESS, D) of
		{value, RequestedIP} ->
		    {init_reboot, RequestedIP};
		false ->
		    case ?is_broadcast(D) of
			false ->
			    {renewing, D#dhcp_server.ciaddr};
			_ ->
			    {rebinding, D#dhcp_server.ciaddr}
		    end
	    end
    end.

-define(reply(DHCP), {reply, DHCP}).
reply(MsgType, D, Opts, #state{server_id = ServerId}) ->
    {reply, D#dhcp_server{
	      op = ?BOOTREPLY,
	      hops = 0,
	      secs = 0,
	      options = [{?DHO_DHCP_MESSAGE_TYPE, MsgType},
			 {?DHO_DHCP_SERVER_IDENTIFIER, ServerId} |
			 Opts]}}.

offer(D, IP, Options, State = #state{next_server = NextServer}) ->
    'Elixir.DHCPServer.Logger':debug(io_lib:format("DHCPOFFER on ~s to ~s ~s ~s",
	       [fmt_ip(IP), fmt_clientid(D),
		fmt_hostname(D), fmt_gateway(D)])),
    reply(?DHCPOFFER, D#dhcp_server{ciaddr = ?INADDR_ANY,
			     yiaddr = IP,
			     siaddr = NextServer
			    },
	  Options, State).

ack(D, IP, Options, State = #state{next_server = NextServer}) ->
    'Elixir.DHCPServer.Logger':debug(io_lib:format("DHCPACK on ~s to ~s ~s ~s",
			  [fmt_ip(IP), fmt_clientid(D),
			   fmt_hostname(D), fmt_gateway(D)])),

    reply(?DHCPACK, D#dhcp_server{yiaddr = IP,
			   siaddr = NextServer
			  },
	  Options, State).

nak(D, Reason, State) ->
    'Elixir.DHCPServer.Logger':debug(io_lib:format("DHCPNAK to ~s ~s ~s. ~s",
			  [fmt_clientid(D), fmt_hostname(D),
			   fmt_gateway(D), Reason])),
    reply(?DHCPNAK, D#dhcp_server{ciaddr = ?INADDR_ANY,
			   yiaddr = ?INADDR_ANY,
			   siaddr = ?INADDR_ANY,
			   flags = D#dhcp_server.flags bor 16#8000  %% set broadcast bit
			  },
	  [{?DHO_DHCP_MESSAGE, Reason}], State).

send_reply(Source, MsgType, Reply, State) ->
    {DstIP, DstPort} = get_dest(Source, MsgType, Reply, State),
    'Elixir.DHCPServer.Logger':debug(io_lib:format("Sending DHCP Reply to: ~s:~w", [fmt_ip(DstIP), DstPort])),
    gen_udp:send(State#state.socket, DstIP, DstPort, dhcp_server_lib:encode(Reply)).

arp_inject_nif(_IfName, _IP, _Type, _Addr, _FD) -> error(nif_not_loaded).

arp_inject(IP, Type, Addr, #state{if_name = IfName, socket = Socket}) ->
    {ok, FD} = inet:getfd(Socket),
    'Elixir.DHCPServer.Logger':debug(io_lib:format("FD: ~w", [FD])),
    arp_inject_nif(IfName, dhcp_server_lib:ip_to_binary(IP), Type, dhcp_server_lib:eth_to_binary(Addr), FD).

%%% Behaviour is described in RFC2131 sec. 4.1
get_dest(Source = {SrcIP, SrcPort}, MsgType, Reply, State)
  when is_record(Reply, dhcp_server) ->
    if Reply#dhcp_server.giaddr =/= ?INADDR_ANY ->
	    'Elixir.DHCPServer.Logger':debug("get_dest: #1"),
	    {Reply#dhcp_server.giaddr, ?DHCP_SERVER_PORT};

       Reply#dhcp_server.ciaddr =/= ?INADDR_ANY ->
	    'Elixir.DHCPServer.Logger':debug("get_dest: #2"),
	    if (MsgType =/= ?DHCPINFORM andalso SrcIP =/= Reply#dhcp_server.ciaddr)
	       orelse SrcIP == ?INADDR_ANY orelse SrcPort == 0 ->
		    {Reply#dhcp_server.ciaddr, ?DHCP_CLIENT_PORT};
	       true ->
		    Source
	    end;

       ?is_broadcast(Reply) ->
	    'Elixir.DHCPServer.Logger':debug("get_dest: #3"),
	    {?INADDR_BROADCAST, ?DHCP_CLIENT_PORT};

       Reply#dhcp_server.yiaddr =/= ?INADDR_ANY ->
	    'Elixir.DHCPServer.Logger':debug("get_dest: #4"),
	    arp_inject(Reply#dhcp_server.yiaddr, Reply#dhcp_server.htype, Reply#dhcp_server.chaddr, State),
	    {Reply#dhcp_server.yiaddr, ?DHCP_CLIENT_PORT};

       true ->
	    'Elixir.DHCPServer.Logger':debug("get_dest: #5"),
	    Source
    end.

optsearch(Option, D) when is_record(D, dhcp_server) ->
    case lists:keysearch(Option, 1, D#dhcp_server.options) of
	{value, {Option, Value}} ->
	    {value, Value};
	false ->
	    false
    end.

get_client_id(D) when is_record(D, dhcp_server) ->
    case optsearch(?DHO_DHCP_CLIENT_IDENTIFIER, D) of
        {value, ClientId} ->
	    ClientId;
	false ->
	    D#dhcp_server.chaddr
    end.

get_requested_ip(D) when is_record(D, dhcp_server) ->
    case optsearch(?DHO_DHCP_REQUESTED_ADDRESS, D) of
        {value, IP} ->
	    IP;
	false ->
	    ?INADDR_ANY
    end.

fmt_clientid(D) when is_record(D, dhcp_server) ->
    fmt_clientid(get_client_id(D));
fmt_clientid([_T, E1, E2, E3, E4, E5, E6]) ->
    fmt_clientid({E1, E2, E3, E4, E5, E6});
fmt_clientid({E1, E2, E3, E4, E5, E6}) ->
    lists:flatten(
      io_lib:format("~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b",
	     [E1, E2, E3, E4, E5, E6])).

fmt_gateway(D) when is_record(D, dhcp_server) ->
    case D#dhcp_server.giaddr of
	?INADDR_ANY -> [];
	IP          -> lists:flatten(io_lib:format("via ~s", [fmt_ip(IP)]))
    end.

fmt_hostname(D) when is_record(D, dhcp_server) ->
    case optsearch(?DHO_HOST_NAME, D) of
        {value, Hostname} ->
            lists:flatten(io_lib:format("(~s)", [Hostname]));
	false ->
	    []
    end.

fmt_ip({A1, A2, A3, A4}) ->
    io_lib:format("~w.~w.~w.~w", [A1, A2, A3, A4]).

get_nsopts(NetNameSpace, Opts)
  when is_binary(NetNameSpace); is_list(NetNameSpace) ->
    [{netns, NetNameSpace} | Opts];
get_nsopts(_, Opts) ->
    Opts.


get_ifopts(Interface, Opts) when is_binary(Interface) ->
    %% setsockopt(s, SOL_SOCKET, SO_BINDTODEVICE, nic, IF_NAMESIZE);
    [{raw, 1, 25, Interface} | Opts].

get_fdopts(Opts) ->
    case init:get_argument(fd) of
	{ok, [[FD]]} ->
	    [{fd, list_to_integer(FD)} | Opts];
	error ->
	    Opts
    end.

get_sockopt(NetNameSpace, Interface) ->
    Opts = [binary, {broadcast, true}],
    Opts0 = get_nsopts(NetNameSpace, Opts),
    Opts1 = get_ifopts(Interface, Opts0),
    get_fdopts(Opts1).
