defmodule IErl.MixProject do
  use Mix.Project
  import Mix.Rebar

  def project do
    args = get_app_src()
    config = load_config(".")

    [
      app: :ierl,
      version: :erlang.list_to_binary(args[:vsn]),
      elixir: "~> 1.4",
      deps: deps(config),
      erlc_options: config[:erl_opts],
      escript: [
        main_module: :ierl,
        app: nil,
        embed_elixir: true
      ]
    ]
  end

  def application do
    args = get_app_src()

    [
      applications: args[:applications]
    ]
  end

  defp get_app_src do
    {:ok,
      [{:application, :ierl, args}]
    } = :file.consult "src/ierl.app.src"

    args
  end
end
