defmodule IErl.Cli.MixProject do
  use Mix.Project
  import Mix.Rebar

  def project do
    args = get_app_src()
    config = load_config(".")

    [
      app: :ierl,
      version: :erlang.list_to_binary(args[:vsn]),
      elixir: "~> 1.4",
      deps: deps(config) ++ [{:jupyter, in_umbrella: true}],
      erlc_options: config[:erl_opts],
      escript: [
        main_module: :ierl,
        app: :ierl,
        embed_elixir: true
      ],
      build_path: "../../_build",
      # config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock"
    ]
  end

  def application do
    args = get_app_src()
    apps = args[:applications]
    # All of Elixir, only elixir and iex are required, though
    ex_apps = [:elixir, :iex, :mix, :logger, :ex_unit]

    [
      applications: apps ++ ex_apps
    ]
  end

  defp get_app_src do
    {:ok, [{:application, :ierl, args}]} = :file.consult("src/ierl.app.src")

    args
  end
end
