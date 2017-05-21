-module(jup_kernel_backend_stub).

-behaviour(jup_kernel_backend).


-export([
         init/1,
         do_execute/4
        ]).


-record(state, {
          bindings
         }).


init(_Args) ->
    #state{
       bindings=erl_eval:new_bindings()
      }.


do_execute(Code, _Publish, Msg, State) ->
    Res = try
              {ok, evaluate(binary_to_list(Code))}
          catch
              Type:Reason ->
                  lager:error("Error: ~p:~p", [Type, Reason]),
                  {error, #{ type => Type, reason => Reason }}
          end,

    Res.


do_is_complete(Code, _Msg, _State) ->
    complete.


evaluate(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    Result.
