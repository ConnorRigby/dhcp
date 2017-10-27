defmodule BuildLib do
  def build do
    res = File.read!("lib.txt")
    |> split()
    |> strip_comments()
    |> transpose()
    # |> sort()
    file = """
defmodule DHCPServer.Lib do
  defmacro __using__(_) do
    quote do
      #{Enum.join(res, "\r\n")}
    end
  end
end
    """
    File.write!("lib/dhcp_server/dhcp_server_lib.ex", file)
  end

  defp split(bin) do
    String.split(bin, "\n")
  end

  defp strip_comments(list, acc \\ [])
  defp strip_comments([], acc), do: Enum.reverse(acc)
  defp strip_comments([line | rest], acc) do
    strip_comments(rest, [strip_comments_from_line(line) | acc])
  end

  defp strip_comments_from_line(line, acc \\ "")
  defp strip_comments_from_line(<<>>, acc), do: acc
  defp strip_comments_from_line(<<"%", rest :: binary>>, acc), do: acc
  defp strip_comments_from_line(<<char, rest :: binary>>, acc) do
    strip_comments_from_line(rest, acc <> <<char>>)
  end

  defp transpose(list, acc \\ [])
  defp transpose([], acc), do: Enum.reverse(acc)
  defp transpose([line | rest], acc) do
    transpose(rest, [transpose_line(line)  | acc] |> List.flatten())
  end

  defp transpose_line(""), do: [""]

  defp transpose_line(<<"-define(", rest :: binary>> = line) do
    [key, rest] = String.split(rest, ", ")
    key = String.downcase(key)
    [val, _]    = String.split(rest, ")")
    val = case Integer.parse(String.trim(val)) do
      {num, _} -> num
      _ -> val
    end
    ["@#{key} #{val}"]
    # ["  def encode(:#{key}), do: #{val}", "  def decode(#{val}), do: :#{key}"]
  end

  defp sort(list, decode_list \\ [], encode_list \\ [])
  defp sort([], dec, enc), do: Enum.reverse(dec) ++ ["\n"] ++ Enum.reverse(enc)
  defp sort([<<"  def decode", _ :: binary>> = line | rest], decode_list, encode_list) do
    sort(rest, [line | decode_list], encode_list)
  end
  defp sort([<<"  def encode", _ :: binary>> = line | rest], decode_list, encode_list) do
    sort(rest, decode_list, [line | encode_list])
  end

  defp sort(["" | rest], dec, enc) do
    sort(rest, dec, enc)
  end
end

BuildLib.build()
