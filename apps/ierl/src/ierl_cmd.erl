-module(ierl_cmd).

-callback exec(_, _, _, _) -> _.
-callback opt_spec() -> _.
