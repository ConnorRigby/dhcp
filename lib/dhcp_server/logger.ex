defmodule DHCPServer.Logger do
  @moduledoc false
  require Logger

  @doc false
  def warn(chardata_or_fun, metadata \\ []) do
    Logger.warn(chardata_or_fun, metadata)
  end

  @doc false
  def info(chardata_or_fun, metadata \\ []) do
    Logger.info(chardata_or_fun, metadata)
  end

  @doc false
  def debug(chardata_or_fun, metadata \\ []) do
    Logger.debug(chardata_or_fun, metadata)
  end

  @doc false
  def error(chardata_or_fun, metadata \\ []) do
    Logger.error(chardata_or_fun, metadata)
  end
end
