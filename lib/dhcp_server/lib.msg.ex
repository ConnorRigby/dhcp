defmodule DHCPServer.Lib.Msg do
  @moduledoc "Encode and Decode DHCP data."

  use DHCPServer.Lib

  defstruct [
    op: nil,
    htype: @htype_ether,
    hlen: 6,
    hops: 0,
    xid: 0,
    secs: 0,
    flags: 0,
    ciaddr: {0,0,0,0},
    yiaddr: {0,0,0,0},
    siaddr: {0,0,0,0},
    giaddr: {0,0,0,0},
    chaddr: {0,0,0,0,0,0},
    sname: [],
    file: [],
    options: []
  ]

  @doc "Decode a DHCP message."
  def decode(<<op, htype, hlen, hops, xid :: size(32), secs :: size(16), flags :: size(16),
    ciaddr  :: binary-size(4),
    yiaddr  :: binary-size(4),
    siaddr  :: binary-size(4),
    giaddr  :: binary-size(4),
    chaddr  :: binary-size(6),
    _       :: binary-size(10),
    sname   :: binary-size(64),
    file    :: binary-size(128),
    options :: binary
    >>
  ) do

    opts = case options do
      <<99, 130, 83, 99, opts :: binary>> ->
        binary_to_options(opts)
      _ -> []
    end

    %__MODULE__{
      op:      op,
      htype:   htype,
      hlen:    hlen,
      hops:    hops,
      xid:     xid,
      secs:    secs,
      flags:   flags,
      ciaddr:  binary_to_ip(ciaddr),
      yiaddr:  binary_to_ip(yiaddr),
      siaddr:  binary_to_ip(siaddr),
      giaddr:  binary_to_ip(giaddr),
      chaddr:  binary_to_eth(chaddr),
      sname:   :erlang.binary_to_list(sname),
      file:    :erlang.binary_to_list(file),
      options: opts
    }
  end

  @doc "Encode a DHCP message."
  def encode(%__MODULE__{
    op: op,
    htype: htype,
    hlen: hlen,
    hops: hops,
    xid: xid,
    secs: secs,
    flags: flags,
    ciaddr: ciaddr,
    yiaddr: yiaddr,
    siaddr: siaddr,
    giaddr: giaddr,
    chaddr: chaddr,
    sname: sname,
    file: file,
    options: opts
  }) do
    ciaddr = ip_to_binary(ciaddr)
    yiaddr = ip_to_binary(yiaddr)
    siaddr = ip_to_binary(siaddr)
    giaddr = ip_to_binary(giaddr)
    chaddr = pad(eth_to_binary(chaddr), 16)
    sname = pad(:erlang.list_to_binary(sname), 64)
    file = pad(:erlang.list_to_binary(file), 128)
    opts = options_to_binary(opts)
    <<op, htype, hlen, hops, xid :: size(32), secs :: size(16), flags :: size(16),
      ciaddr :: binary, yiaddr :: binary, siaddr :: binary, giaddr :: binary,
      chaddr :: binary, sname :: binary, file :: binary, opts :: binary>>
  end

  # IP <-> Binary conversion.

  @doc "Binary to ip address."
  def binary_to_ip(<<a, b, c, d>>), do: {a, b, c, d}

  @doc "Ip address to binary."
  def ip_to_binary({a, b, c, d}), do: <<a, b, c, d>>

  # Eth <> Binary conversion.

  @doc "Binary to eth."
  def binary_to_eth(<<a, b, c, d, e, f>>), do: {a, b, c, d, e, f}

  @doc "Eth to binary."
  def eth_to_binary({a, b, c, d, e, f}), do: <<a, b, c, d, e, f>>

  # IPList <> Binary conversion.

  @doc "Binary to iplist."
  def binary_to_iplist(<<a, b, c, d, t :: binary>>), do: [{a, b, c, d} | binary_to_iplist(t)]

  def binary_to_iplist(<<>>), do: []

  # Shortlist <> Binary conversion.

  @doc "Binary to shortlist."
  def binary_to_shortlist(<<h:: size(16), t :: binary>>) do
    [h | binary_to_shortlist(t)]
  end

  def binary_to_shortlist(<<>>), do: []

  @doc "Binary to options."
  def binary_to_options(binary, acc \\ [])

  def binary_to_options(<<@dho_end, _ :: binary()>>, acc) do
    acc
  end

  def binary_to_options(<<tag, rest :: binary>>, acc) do
    {value, t} = case type(tag) do
      :byte ->
        <<1, byte, t :: binary >> = rest
        {byte, t}

      :short ->
        <<2, short :: size(16), t :: binary>> = rest
        {short, t}

      :shortlist ->
        <<n, bin :: size(n)-binary, t :: binary>> = rest
        {binary_to_shortlist(bin), t}

      :integer ->
        <<4, int :: size(32), t :: binary>> = rest
        {int, t}

      :string ->
        <<n, str :: size(n)-binary, t :: binary>> = rest
        {:erlang.binary_to_list(str), t}

      :ip ->
        <<4, a, b, c, d, t :: binary>> = rest
        {{a, b, c, d}, t}

      :iplist ->
        <<n, bin :: size(n)-binary, t :: binary>> = rest
        {binary_to_iplist(bin), t}

      :vendor ->
        <<n, bin :: size(n)-binary, t :: binary>> = rest
        {binary_to_options(bin), t}

      :unknown ->
        <<n, bin :: size(n)-binary, t :: binary>> = rest
        {bin, t}
    end
    binary_to_options(t, [{tag, value} | acc])
  end

  @doc "Options to binary."
  def options_to_binary(opts) do
    l = for {tag, val} <- opts, do: <<(option_to_binary(tag, val)) :: binary>>
    :erlang.list_to_binary(@dhcp_options_cookie ++ l ++ [@dho_end])
  end

  @doc "Single option to binary."
  def option_to_binary(tag, val) do
    case type(tag) do
      :byte -> <<tag, 1, val>>

      :short -> <<tag, 2, val :: size(16)-big>>

      :shortlist ->
        b = :erlang.list_to_binary(for s <- val, do: <<s :: size(16)-big>>)
        <<tag, (byte_size(b)), b :: binary>>

      :integer -> <<tag, 4, val :: size(32)-big>>

      :string ->
        b = :erlang.list_to_binary(val)
        <<tag, (byte_size(b)), b :: binary>>

      :ip -> <<tag, 4, (ip_to_binary(val)) :: binary>>

      :iplist ->
        b = :erlang.list_to_binary(for ip <- val, do: ip_to_binary(ip))
        <<tag, (byte_size(b)), b :: binary>>

      :vendor ->
        b = :erlang.list_to_binary(for {t, v} <- val, do: <<t, (byte_size(v)), v :: binary>>)
        <<tag, (byte_size(b)), b :: binary>>
    end
  end

  defp pad(x, size) when is_binary(x) do
    len = byte_size(x)
    plen = size - len
    <<x :: binary, 0:: size(plen)-integer-unit(8)>>
  end
end
