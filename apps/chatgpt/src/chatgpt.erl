-module(chatgpt).

-export([start/0]).

-ignore_xref([start/0]).

-include_lib("include/logger.hrl").

start() ->
    {ok, _} = application:ensure_all_started(chatgpt),
    ok.
