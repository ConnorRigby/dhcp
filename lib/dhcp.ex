defmodule Dhcp do
  @moduledoc false
  def hello do
    Application.put_env(:dhcp, :some_key, "HEY LOOK AT ME")
  end
end
