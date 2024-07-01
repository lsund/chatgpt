-module(user_default).

-compile(nowarn_export_all).
-compile(export_all).

-define(FWD0(Name), Name() ->  chatgpt:Name()).

?FWD0(start).
