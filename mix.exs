defmodule Dhcp.Mixfile do
  use Mix.Project

  def project do
    [
      app: :dhcp,
      version: "0.0.1",
      elixir: "~> 1.4",
      compilers: Mix.compilers ++ [:elixir_make],
      make_cwd: "c_src",
      make_env: %{
        "CFLAGS" => "-O2 -I#{:code.root_dir()}/erts-#{:erlang.system_info(:version)}/include"
      },
      erlc_options: [{:parse_transform, :lager_transform}],
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: []
    ]
  end

  def deps do
    [
      {:elixir_make, "~> 0.4", runtime: false},
      {:lager, ">= 2.1.0", manager: :rebar3}
    ]
  end
end
