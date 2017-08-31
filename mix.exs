defmodule DHCPServer.Mixfile do
  use Mix.Project

  def project do
    [
      app: :dhcp_server,
      version: "0.1.0",
      elixir: "~> 1.4",
      compilers: Mix.compilers ++ [:elixir_make],
      package: package(),
      description: description(),
      make_clean: ["clean"],
      erlc_options: [{:parse_transform}],
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      applications: [:logger]
    ]
  end

  def deps do
    [
      {:elixir_make, "~> 0.4", runtime: false},
      {:ex_doc, "~> 0.14", only: :dev},
      {:nerves_network, ">= 0.0.0", [optional: true]}
    ]
  end

  defp description do
    """
    Configure a linux network interface to act as a DHCP Server.
    """
  end

  defp package do
    [
      files: ["lib", "priv", "mix.exs", "README*", "COPYING", "src", "c_src"],
      maintainers: ["Connor Rigby"],
      licenses: ["GPLv2"],
      links: %{"GitHub" => "https://github.com/nerves-project/dhcp_server"}
    ]
  end
end
