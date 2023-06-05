-module(chatgpt_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("include/logger.hrl").

stop(_State) -> ok.

start(_, _) ->
    {ok, Pid} = chatgpt_sup:start_link(),
    chatgpt_sup:restart_and_query(),
    {ok, Pid}.
