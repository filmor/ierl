-ifndef(JUPYTER_INTERNAL_HRL).
-define(JUPYTER_INTERNAL_HRL, 1).

-include("records.hrl").

-record(jup_msg, {
          uuids = []          :: [binary()],
          header = #{}        :: map(),
          parent_header = #{} :: map(),
          metadata = #{}      :: map(),
          content = #{}       :: map(),
          extra_binaries = [] :: [binary()]
         }).


-define(JUP_VIA(Name, SubName), {via, jup_registry, {Name, SubName}}).
-define(JUP_NAME(Name, SubName), {n, l, {jupyter, Name, SubName}}).

-endif.
