defmodule DHCPServer.Application do
  @moduledoc false
  use Supervisor

  @doc false
  def start(_type, args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(args) do
    children = []
    opts = [strategy: :one_for_one]
    supervise(children, opts)
  end
end
