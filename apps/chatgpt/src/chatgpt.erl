-module(chatgpt).

-export([start/0, restart/0]).

-ignore_xref([start/0, restart/0]).

-include_lib("include/logger.hrl").

start() ->
    {ok, _} = application:ensure_all_started(chatgpt),
    ok.

restart() ->
    {ok, _} = application:ensure_all_started(chatgpt),
    chatgpt_sup:restart_and_query().
