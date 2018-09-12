-ifndef(JUPYTER_INTERNAL_HRL).
-define(JUPYTER_INTERNAL_HRL, 1).

-include("records.hrl").

-record(jup_msg, {
          uuids = []          :: [binary()],
          header = #{}        :: map(),
          type = <<>>         :: binary(),
          parent_header = #{} :: map(),
          metadata = #{}      :: map(),
          content = #{}       :: map(),
          extra_binaries = [] :: [binary()]
         }).


-define(JUP_VIA(Name, SubName), {via, jup_registry, {Name, SubName}}).
-define(JUP_NAME(Name, SubName), {n, l, {jupyter, Name, SubName}}).
-define(JUP_PROTO_VERSION, <<"5.3">>).

% -define(DEBUG, 1).
-ifdef(DEBUG).
-define(LOG(Severity, Msg, Args), lager:log(Severity, self(), Msg, Args)).
-else.
-define(LOG(Severity, Msg, Args), ok).
-endif.

-endif.
