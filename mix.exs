defmodule DHCPServer.Mixfile do
  use Mix.Project

  def project do
    [
      app: :dhcp_server,
      version: "0.0.1",
      elixir: "~> 1.4",
      compilers: Mix.compilers ++ [:elixir_make],
      make_env: %{
        "ERTS_DIR" => "#{:code.root_dir()}/erts-#{:erlang.system_info(:version)}/"
      },
      make_clean: ["clean"],
      erlc_options: [{:parse_transform, :lager_transform}],
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {:dhcp_server_app, []},
      applications: [:kernel, :logger]
    ]
  end

  def deps do
    [
      {:elixir_make, "~> 0.4", runtime: false},
    ]
  end
end
