defmodule IErl.Jupyter.MixProject do
  use Mix.Project
  import Mix.Rebar

  def project do
    args = get_app_src()
    config = load_config(".")

    [
      app: :jupyter,
      version: :erlang.list_to_binary(args[:vsn]),
      elixir: "~> 1.4",
      deps: deps(config),
      erlc_options: config[:erl_opts],
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock"
    ]
  end

  defp get_app_src do
    {:ok, [{:application, :jupyter, args}]} = :file.consult("src/jupyter.app.src")

    args
  end
end
