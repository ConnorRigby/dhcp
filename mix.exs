defmodule Dhcp.Mixfile do
  use Mix.Project

  def project do
    [
      app: :dhcp,
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

  def deps do
    [
      {:elixir_make, "~> 0.4", runtime: false},
      {:lager, ">= 2.1.0", manager: :rebar3}
    ]
  end
end
