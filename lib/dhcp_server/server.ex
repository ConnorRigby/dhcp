defmodule DHCPServer.Server do
  @moduledoc "Handles the  UDP connection."

  use GenServer
  require Logger
  use DHCPServer.Lib
  alias DHCPServer.Lib.Msg
  require Bitwise

  if Mix.Project.config[:target] == System.get_env("NERVES_TARGET") do
    @on_load :load_nif
    @doc false
    def load_nif do
      nif_file = :filename.join(:code.priv_dir(:dhcp_server), 'dhcp_server')
      case :erlang.load_nif(nif_file, 0) do
        :ok -> :ok
        {:error, {:reload, _}} -> :ok
        {:error, reason} -> exit(reason)
      end
    end
  end

  ## GenServer

  defmodule State do
    @moduledoc false
    defstruct [:ifname, :socket, :server_id, :next_server]
  end

  @doc "Start the DHCP Server."
  def start_link(net_name_space, interface, server_id, next_server) do
    GenServer.start_link(__MODULE__, [net_name_space, interface, server_id, next_server])
  end

  def init([net_name_space, interface, server_id, next_server]) do
    opts = get_sockopt(net_name_space, interface)
    case :gen_udp.open(@dhcp_server_port, opts) do
      {:ok, socket}    -> {:ok, %State{socket: socket, ifname: interface, server_id: server_id, next_server: next_server}}
      {:error, reason} -> {:stop, reason}
    end
  end

  def handle_cast(_msg, state), do: {:noreply, state}

  def handle_info({:udp, _socket, ip, port, packet}, %State{} = state) do
    source = {ip, port}
    request = Msg.decode(packet)
    case optsearch(request, @dho_dhcp_message_type) do
      false -> :ok
      {:value, type} ->
        case handle_dhcp(type, request, state) do
          :ok -> {:noreply, state}
          {:reply, reply} ->
            send_reply(source, type, reply, state)
            {:noreply, state}
          # {:reply, type, msg, opts, state} ->
            # send_reply(source, type, msg, state)
            # {:noreply, state}
          {:error, reason} ->
            {:exit, reason, state}
          other ->
            Logger.debug ("DHCP result: #{inspect other}")
            {:noreply, state}
        end
    end
  end

  def terminate(_, state) do
    :gen_udp.close(state.socket)
  end

  ## Private

  defp handle_dhcp(type, request, state)

  defp handle_dhcp(@dhcpdiscover, req, state) do
    Logger.debug "DHCPDISCOVER from #{fmt_clientid(req)} #{fmt_hostname(req)} #{fmt_gateway(req)}"
    client_id = get_client_id(req)
    gateway = req.giaddr
    requested_ip = get_requested_ip(req)
    case :dhcp_server_alloc.reserve(state.ifname, client_id, gateway, requested_ip) do
      {:ok, ip, opts} ->
        offer(req, ip, opts, state)
      other -> other
    end
  end

  defp handle_dhcp(@dhcprequest, req, state) do
    Logger.debug "DHCPREQUEST from #{fmt_clientid(req)} #{fmt_hostname(req)} #{fmt_gateway(req)}"
    client_id = get_client_id(req)
    gateway = req.giaddr

    case client_state(req) do

      {:selecting, server_id} ->
        case {server_id, state.server_id} do
          {a, b} when a == b ->
            ip = get_requested_ip(req)
            case :dhcp_server_alloc.allocate(state.ifname, client_id, ip) do
              {:ok, ip, opts} -> ack(req, ip, opts, state)
              other -> other
            end
          _ -> :ok
        end

      {:init_reboot, requested_ip} ->
        case :dhcp_server_alloc.verify(state.ifname, client_id, gateway, requested_ip) do
          {:ok, ip, opts} -> ack(req, ip, opts, state)

          {:error, reason} -> nak(req, reason, state)

          :nolease ->
            Logger.warn "No lease for #{fmt_ip(requested_ip)}. sending a new one."
            case :dhcp_server_alloc.reserve(state.ifname, client_id, gateway, requested_ip) do
              {:ok, _ip, _opts} ->
                case :dhcp_server_alloc.allocate(state.ifname, client_id, requested_ip) do
                  {:ok, _ip, _opts} ->
                    case :dhcp_server_alloc.verify(state.ifname, client_id, gateway, requested_ip) do
                      {:ok, ip, opts} -> ack(req, ip, opts, state)
                      other -> other
                    end
                  other -> other
                end
              other -> other
            end

          :noclient ->
            Logger.warn "Client #{fmt_clientid(req)} has no current bindings."
            :ok
        end

      {client_is, ip} when client_is == :renewing or client_is == :rebinding ->
        case :dhcp_server_alloc.extend(state.ifname, client_id, ip) do
          {:ok, ip, opts}  -> ack(req, ip, opts, state)
          {:error, reason} -> nak(req, reason, state)
        end

    end
  end

  defp handle_dhcp(@dhcpdecline, req, state) do
    ip = get_requested_ip(req)
    client_id = get_client_id(req)
    Logger.debug "DHCPDECLINE of #{fmt_ip(ip)} from #{fmt_clientid(req)} #{fmt_hostname(req)}"
    :dhcp_server_alloc.decline(state.ifname, client_id, ip)
  end

  defp handle_dhcp(@dhcprelease, req, state) do
    client_id = get_client_id(req)
    Logger.debug("DHCPRELEASE of #{fmt_ip(req.ciaddr)} from #{fmt_clientid(req)} #{fmt_hostname(req)} #{fmt_gateway(req)}")
    :dhcp_server_alloc.release(state.ifname, client_id, req.ciaddr)
  end

  defp handle_dhcp(@dhcpinform, req, state) do
    gateway = req.giaddr
    ip = req.ciaddr
    Logger.debug("DHCPINFORM from #{fmt_ip(ip)}")
    case :dhcp_server_alloc.local_conf(state.ifname, gateway) do
      {:ok, opts} ->
        opts_sans_lease = :lists.keydelete(@dho_dhcp_lease_time, 1, opts)
        ack(req, ip, opts_sans_lease, state)
      other -> other
    end
  end

  defp handle_dhcp(msg_type, _req, _state) do
    Logger.warn("Invalid DHCP message type: #{msg_type}")
    :ok
  end

  defp client_state(%Msg{} = req) do
    case optsearch(req, @dho_dhcp_server_identifier) do
      {:value, server_id} -> {:selecting, server_id}
      false ->
        case optsearch(req, @dho_dhcp_requested_address) do
          {:value, requested_ip} -> {:init_reboot, requested_ip}
          false ->
            case is_broadcast(req) do
              false -> {:renewing, req.ciaddr}
              true  -> {:rebinding, req.ciaddr}
            end
        end
    end
  end

  defp reply(msg_type, %Msg{} = req, opts, %State{server_id: server_id}) do
    {:reply, %Msg{ req |
      op: @bootreply,
      hops: 0,
      secs: 0,
      options: [ {@dho_dhcp_message_type, msg_type}, {@dho_dhcp_server_identifier, server_id} | opts]
      }}
  end

  defp offer(%Msg{} = req, ip, options, %State{next_server: next_server} = state) do
    Logger.debug "DHCPOFFER on #{fmt_ip(ip)} to #{fmt_clientid(req)} #{fmt_hostname(req)} #{fmt_gateway(req)}"
    reply @dhcpoffer, %Msg{ req |
      ciaddr: @inaddr_any,
      yiaddr: ip,
      siaddr: next_server
      }, options, state
  end

  defp ack(%Msg{} = req, ip, options, %State{next_server: next_server} = state) do
    Logger.debug "DHCPACK on #{fmt_ip(ip)} to #{fmt_clientid(req)} #{fmt_hostname(req)} #{fmt_gateway(req)}"
    reply @dhcpack, %Msg{req | yiaddr: ip, siaddr: next_server }, options, state
  end

  defp nak(%Msg{} = req, reason, state) do
    Logger.debug "DHCPNAK to #{fmt_clientid(req)} #{fmt_hostname(req)} #{fmt_gateway(req)} #{inspect reason}"
    reply @dhcpnak, %Msg{req |
      ciaddr: @inaddr_any,
      yiaddr: @inaddr_any,
      siaddr: @inaddr_any,
      flags: Bitwise.bor(req.flags, 0x8000)
    }, [{@dho_dhcp_message, reason}], state
  end

  defp send_reply(source, msg_type, reply, state) do
    {dst_ip, dst_port} = get_dest(source, msg_type, reply, state)
    Logger.debug "sending DHCP Reply to: #{fmt_ip(dst_ip)}:#{dst_port}"
    :ok = :gen_udp.send(state.socket, dst_ip, dst_port, Msg.encode(reply))
  end

  defp arp_inject(ip, type, addr, %State{ifname: ifname, socket: socket}) do
    {:ok, fd} = :inet.getfd(socket)
    Logger.debug "FD: #{inspect fd}"
    arp_inject_nif(ifname, Msg.ip_to_binary(ip), type, Msg.eth_to_binary(addr), fd)
  end

  def arp_inject_nif(_,_, _, _, _), do: {:error, :no_nif}

  defp get_dest({src_ip, src_port} = source, msg_type, %Msg{} = reply, %State{} = state) do
    cond do
      reply.giaddr != @inaddr_any ->
        Logger.debug "get_dest: #1"
        {reply.giaddr, @dhcp_server_port}

      reply.ciaddr != @inaddr_any ->
        Logger.debug "get_dest: #2"
        if (msg_type != @dhcpinform) and (src_ip != reply.ciaddr) or (src_ip == @inaddr_any) or src_port == 0 do
          {reply.ciaddr, @dhcp_client_port}
        else
          source
        end

      is_broadcast(reply) ->
        Logger.debug "get_dest: #3"
        {@inaddr_broadcast, @dhcp_client_port}

      reply.yiaddr != @inaddr_any ->
        Logger.debug "get_dest: #4"
        arp_inject(reply.yiaddr, reply.htype, reply.chaddr, state)
        {reply.yiaddr, @dhcp_client_port}

      true ->
        Logger.debug "get_dest #5"
        source
    end
  end

  defp get_sockopt(net_name_space, interface) do
    [:binary, {:broadcast, true}]
      |> get_nsopts(net_name_space)
      |> get_ifopts(interface)
  end

  defp get_nsopts(opts, net_name_space) when is_binary(net_name_space) and is_list(opts) do
    [{:netns, net_name_space} | opts]
  end

  defp get_nsopts(opts, _), do: opts

  defp get_ifopts(opts, interface) when is_binary(interface) do
    [{:raw, 1, 25, interface} | opts]
  end

  defp optsearch(%Msg{options: opts}, opt) do
    case :lists.keysearch(opt, 1, opts) do
      {:value, {^opt, value}} -> {:value, value}
      false -> false
    end
  end

  defp get_client_id(%Msg{} = msg) do
    case optsearch(msg, @dho_dhcp_client_identifier) do
      {:value, client_id} -> client_id
      false -> msg.chaddr
    end
  end

  defp get_requested_ip(%Msg{} = msg) do
    case optsearch(msg, @dho_dhcp_requested_address) do
      {:value, ip} -> ip
      false -> @inaddr_any
    end
  end

  defp fmt_clientid(%Msg{} = msg) do
    fmt_clientid(get_client_id(msg))
  end

  defp fmt_clientid([_T, e1, e2, e3, e4, e5, e6]), do: fmt_clientid({e1, e2, e3, e4, e5, e6});

  defp fmt_clientid({e1, e2, e3, e4, e5, e6}) do
    :io_lib.format('~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b', [e1, e2, e3, e4, e5, e6]) |> List.flatten()
  end

  defp fmt_clientid(_), do: "no client id"

  defp fmt_gateway(%Msg{} = msg) do
    case msg.giaddr do
      @inaddr_any -> ""
      ip -> "via: #{fmt_ip(ip)}"
    end
  end

  defp fmt_hostname(%Msg{} = msg) do
    case optsearch(msg, @dho_host_name) do
      {:value, hostname} -> "(#{hostname})"
      false -> "no host name"
    end
  end

  defp fmt_ip({a, b, c, d}), do: "#{a}.#{b}.#{c}.#{d}"

  defp is_broadcast(%Msg{} = msg) do
    Bitwise.bsr(msg.flags, 15) == 1
  end

end
