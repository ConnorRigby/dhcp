defmodule BuildTypes do
  def build do
    File.read!("type.txt")
    |> String.split("\n")
    |> transpose()
  end

  defp transpose(l, acc \\ [])
  defp transpose([], acc), do: Enum.reverse(acc)
  defp transpose([line | rest], acc) do
    transpose(rest, [transpose_line(line) | acc])
  end

  defp transpose_line(<<"type(?", rest :: binary>> = line) do
    [key, rest] = String.split(rest, ")")
    key = String.downcase(key)

    [_, val] = String.split(rest, "-> ")
    val = String.trim(val, ";")
    "def type(@#{key}), do: :#{val}"
  end
  defp transpose_line(""), do: ""
end

BuildTypes.build()
