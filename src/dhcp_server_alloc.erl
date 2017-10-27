%%%-------------------------------------------------------------------
%%% File    : dhcp_alloc.erl
%%% Author  : Ruslan Babayev <ruslan@babayev.com>
%%% Description : The allocation module of the DHCP server
%%%
%%% Created : 20 Sep 2006 by Ruslan Babayev <ruslan@babayev.com>
%%%-------------------------------------------------------------------
-module(dhcp_server_alloc).

-behaviour(gen_server).

%% API
-export([start_link/4, reserve/4, allocate/3, release/3, verify/4,
	 extend/3, decline/3, local_conf/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([server_name/1, ets_table/1, dets_table/1]).

-include("dhcp_server.hrl").
-include("dhcp_server_alloc.hrl").

-define(DHCPOFFER_TIMEOUT, 3*60*1000).

-define(IS_ALLOCATED(A), A#address.status == allocated).
-define(IS_OFFERED(A), A#address.status == offered).
-define(IS_AVAILABLE(A), A#address.status == available).
-define(IS_DECLINED(A), A#address.status == declined).

-define(IS_NOT_EXPIRED(L, Now), L#lease.expires > Now).

-record(state, {subnets, hosts, interface}).

server_name(Interface) ->
  list_to_atom(atom_to_list(?MODULE) ++ binary_to_list(Interface)).

ets_table(Interface) ->
  list_to_atom(atom_to_list(address) ++ [45] ++ binary_to_list(Interface)).

dets_table(Interface) ->
  list_to_atom(atom_to_list(lease) ++ [45] ++ binary_to_list(Interface)).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Interface, LeaseFile, Subnets, Hosts) ->
    gen_server:start_link({local, server_name(Interface)}, ?MODULE,
			  [Interface, LeaseFile, Subnets, Hosts], []).

reserve(Interface, ClientId, Gateway, IP) ->
    gen_server:call(server_name(Interface), {reserve, ClientId, Gateway, IP}).

allocate(Interface, ClientId, IP) ->
    gen_server:call(server_name(Interface), {allocate, ClientId, IP}).

release(Interface, ClientId, IP) ->
    gen_server:call(server_name(Interface), {release, ClientId, IP}).

verify(Interface, ClientId, Gateway, IP) ->
    gen_server:call(server_name(Interface), {verify, ClientId, Gateway, IP}).

extend(Interface, ClientId, IP) ->
    gen_server:call(server_name(Interface), {extend, ClientId, IP}).

local_conf(Interface, Gateway) ->
    gen_server:call(server_name(Interface), {local_conf, Gateway}).

decline(Interface, ClientId, IP) ->
    gen_server:call(server_name(Interface), {decline, ClientId, IP}).

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
init([Interface, LeaseFile, Subnets, Hosts]) ->
    ets:new(ets_table(Interface), [named_table, public, {keypos, #address.ip}]),
    dets:open_file(dets_table(Interface), [{keypos, #lease.clientid}, {file, LeaseFile}]),
    lists:foreach(fun(X) -> init_subnet(X, Interface) end, Subnets),
    {ok, #state{subnets = Subnets, hosts = Hosts, interface = Interface}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({reserve, ClientId, Gateway, IP}, _From, State) ->
    case select_subnet(Gateway, State#state.subnets) of
      {ok, Subnet} ->
        case select_address(ClientId, IP, Subnet, State#state.interface) of
          {ok, Address} -> {reply, reserve_address(Address, ClientId, State#state.interface), State};
          {error, Reason} -> {reply, {error, Reason}, State}
        end;
      {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({allocate, ClientId, IP}, _From, State) ->
    case ets:lookup(ets_table(State#state.interface), IP) of
      [A] when ?IS_OFFERED(A) -> {reply, allocate_address(A, ClientId, State#state.interface), State};
      _ -> {reply, {error, "Address is not offered."}, State}
    end;

handle_call({release, ClientId, IP}, _From, State) -> {reply, release_address(ClientId, IP, State#state.interface), State};

handle_call({verify, ClientId, Gateway, IP}, _From, State) ->
  case select_subnet(Gateway, State#state.subnets) of
    {ok, Subnet} -> {reply, verify_address(ClientId, IP, Subnet, State#state.interface), State};
    {error, Reason} -> {reply, {error, Reason}, State}
  end;

handle_call({extend, ClientId, IP}, _From, State) ->
    case ets:lookup(ets_table(State#state.interface), IP) of
      [A] when ?IS_ALLOCATED(A) -> {reply, allocate_address(A, ClientId, State#state.interface), State};
      _ -> {reply, {error, "Address is not allocated."}, State}
    end;

handle_call({local_conf, Gateway}, _From, State) ->
  case select_subnet(Gateway, State#state.subnets) of
    {ok, Subnet} -> {reply, {ok, Subnet#subnet.options}, State};
    {error, Reason} -> {reply, {error, Reason}, State}
  end;

handle_call({decline, ClientId, IP}, _From, State) ->
  case ets:lookup(ets_table(State#state.interface), IP) of
    [A] when ?IS_ALLOCATED(A) -> {reply, decline_address(A, ClientId, State#state.interface), State};
    _ -> {reply, {error, "Address not allocated."}, State}
  end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({expired, IP}, State) ->
    Address = #address{ip = IP, status = available},
    ets:insert(ets_table(State#state.interface), Address),
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
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
select_subnet(_, []) -> {error, "No subnet."};

select_subnet({0, 0, 0, 0}, [First | _]) -> {ok, First};

select_subnet(IP, [S|T]) ->
  case belongs_to_subnet(IP, S) of
    true -> {ok, S};
    false -> select_subnet(IP, T)
  end.

belongs_to_subnet(IP, S) ->
  ip_to_int(S#subnet.network) == ip_to_int(IP) band ip_to_int(S#subnet.netmask).

ip_to_int({A,B,C,D}) ->
  <<Int:32>> = <<A,B,C,D>>,
  Int.

int_to_ip(Int) ->
  <<A,B,C,D>> = <<Int:32>>,
  {A,B,C,D}.

select_address(_ClientId, {0, 0, 0, 0}, S, Interface) ->
  F = fun(A, false) when ?IS_AVAILABLE(A) ->
    case belongs_to_subnet(A#address.ip, S) of
      true  -> A;
      false -> false
		end;
	  (_, Acc) -> Acc
  end,
  case ets:foldl(F, false, ets_table(Interface)) of
    false -> {error, "No available addresses in this subnet."};
	  A     -> {ok, A#address{options = S#subnet.options}}
  end;

select_address(ClientId, IP, S, Interface) ->
  Options = S#subnet.options,
  Now = calendar:datetime_to_gregorian_seconds({date(), time()}),
  case belongs_to_subnet(IP, S) of
     false -> select_address(ClientId, {0,0,0,0}, S, Interface);
	   true ->
       case dets:lookup(dets_table(Interface), ClientId) of
         [L] when ?IS_NOT_EXPIRED(L, Now) -> {ok, #address{ip = L#lease.ip, options = Options}};
		     [L] ->
           case ets:lookup(ets_table(Interface), L#lease.ip) of
             [A] when ?IS_AVAILABLE(A) -> {ok, A#address{options = Options}};
             _ ->
               dets:delete(dets_table(Interface), ClientId),
               select_address(ClientId, IP, S, Interface)
           end;
         [] ->
           case ets:lookup(ets_table(Interface), IP) of
             [A] when ?IS_AVAILABLE(A) -> {ok, A#address{options = Options}};
             [A] when ?IS_OFFERED(A) -> {error, "Already offered"};
		         _ -> select_address(ClientId, {0,0,0,0}, S, Interface)
	         end
       end
  end.

verify_address(ClientId, IP, S, Interface) ->
  case belongs_to_subnet(IP, S) of
    true ->
       DateTime = {date(), time()},
	     Now = calendar:datetime_to_gregorian_seconds(DateTime),
	     case dets:lookup(dets_table(Interface), ClientId) of
         [L] when ?IS_NOT_EXPIRED(L, Now), L#lease.ip == IP ->
           allocate_address(ClientId, IP, S#subnet.options, Interface),
		       {ok, IP, S#subnet.options};
		     [_] -> {error, "Address is not currently allocated to lease."};
		     [] -> nolease
	    end;
	  false ->
      release_address(ClientId, IP, Interface),
      {error, "Wrong network."}
  end.

reserve_address(A, ClientId, Interface) ->
  cancel_timer(A#address.timer),
  Timer = erlang:send_after(?DHCPOFFER_TIMEOUT, server_name(Interface), {expired, A#address.ip}),
  ets:insert(ets_table(Interface), A#address{status = offered, timer = Timer}),
  DateTime = {date(), time()},
  Now = calendar:datetime_to_gregorian_seconds(DateTime),
  Expires = Now + ?DHCPOFFER_TIMEOUT,
  Lease = #lease{clientid = ClientId, ip = A#address.ip, expires = Expires},
  dets:insert(dets_table(Interface), Lease),
  {ok, A#address.ip, A#address.options}.

allocate_address(ClientId, IP, Options, Interface) ->
  allocate_address(#address{ip = IP, options = Options}, ClientId, Interface).

allocate_address(A, ClientId, Interface) ->
  IP = A#address.ip,
  Options = A#address.options,
  cancel_timer(A#address.timer),
  case lists:keysearch(?DHO_DHCP_LEASE_TIME, 1, Options) of
     false -> {error, "Lease time not configured."};
	   {value, {?DHO_DHCP_LEASE_TIME, LeaseTime}} ->
       DateTime = {date(), time()},
	     Gregorian = calendar:datetime_to_gregorian_seconds(DateTime),
	     Expires = Gregorian + LeaseTime,
	     Lease = #lease{clientid = ClientId, ip = IP, expires = Expires},
	     dets:insert(dets_table(Interface), Lease),
	     T = erlang:send_after(LeaseTime * 1000, server_name(Interface), {expired, IP}),
	     ets:insert(ets_table(Interface), A#address{status = allocated, timer = T}),
	     {ok, IP, Options}
    end.

release_address(ClientId, IP, Interface) ->
  case ets:lookup(ets_table(Interface), IP) of
    [A] when ?IS_ALLOCATED(A) ->
      IP = A#address.ip,
	    case dets:lookup(dets_table(Interface), ClientId) of
        [L] when L#lease.ip == IP ->
          cancel_timer(A#address.timer),
		      Address = #address{ip = IP, status = available},
		      ets:insert(ets_table(Interface), Address),
		      DateTime = {date(), time()},
		      Now = calendar:datetime_to_gregorian_seconds(DateTime),
		      dets:insert(dets_table(Interface), L#lease{expires = Now}),
		      ok;
        _ -> {error, "Allocated to someone else."}
	    end;
    _ -> {error, "Address not allocated."}
  end.

decline_address(A, ClientId, Interface) ->
  IP = A#address.ip,
  case dets:lookup(dets_table(Interface), ClientId) of
    [L] when L#lease.ip == IP ->
      cancel_timer(A#address.timer),
	    ets:insert(ets_table(Interface), #address{ip = IP, status = declined}),
	    dets:delete(dets_table(Interface), L#lease.clientid),
	    ok;
	  _ -> {error, "Allocated to other lease."}
  end.

cancel_timer(Timer) when is_reference(Timer) -> erlang:cancel_timer(Timer);
cancel_timer(_Timer) -> ok.

init_subnet(Subnet, Interface) ->
  init_available(Subnet#subnet.range, Interface),
  init_allocated(Subnet, Interface).

init_available({X, X}, Interface) ->
  ets:insert(ets_table(Interface), #address{ip = X, status = available});

init_available({Low, High}, Interface) ->
  ets:insert(ets_table(Interface), #address{ip = Low, status = available}),
  NextIP = int_to_ip(ip_to_int(Low) + 1),
  init_available({NextIP, High}, Interface).

init_allocated(#subnet{options = Options}, Interface) ->
  DateTime = {date(), time()},
  Now = calendar:datetime_to_gregorian_seconds(DateTime),
  Allocate = fun(L, Acc) when ?IS_NOT_EXPIRED(L, Now) ->
    allocate_address(L#lease.clientid, L#lease.ip, Options, Interface), [L#lease.clientid | Acc];
		  (_, Acc) -> Acc
  end,
  dets:foldl(Allocate, [], dets_table(Interface)).
