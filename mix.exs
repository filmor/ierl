defmodule IErl.MixProject do
  use Mix.Project
  import Mix.Rebar

  def project do
    config = load_config(".")

    [
      apps_path: "apps",
      # apps: [:ierl, :jupyter],
      elixir: "~> 1.10",
      erlc_options: config[:erl_opts]
    ]
  end
end
