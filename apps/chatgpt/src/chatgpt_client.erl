-include_lib("kernel/include/logger.hrl").

-module(chatgpt_client).

-export([start_link/3, handle_response/2]).

-behaviour(gen_server).

-define(CREDITS, 1).
-define(SOUND_COMMAND, "/usr/bin/mplayer -af scaletempo=scale=3.0 speed=pitch -speed 10 ./public/bell-sound-370341.mp3").
-define(TMP_FILE, "/tmp/chatgpt").

-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3
]).

-export([
    test_os_cmd/0
]).

-record(state, {
    redis_key :: binary(),
    % redis_conn :: pid(),
    data :: any(),
    outfile :: binary()
}).

-record(response, {
    meta :: map(),
    body :: map()
}).

start_link(RedisKey, Data, Outfile) ->
    gen_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        #{redis_key => RedisKey, data => Data, outfile => Outfile},
        []
    ).

init(#{redis_key := RedisKey, data := Data, outfile := Outfile}) ->
    do_requests(RedisKey),
    State = #state{
        redis_key = RedisKey,
        data = Data,
        outfile = Outfile
    },
    {ok, State}.

do_requests(Path) ->
    gen_server:cast(?MODULE, {do_requests, Path}).

handle_response(Meta, Body) ->
    gen_server:cast(
        ?MODULE,
        {handle_response, #response{meta = Meta, body = Body}}
    ).

handle_cast({do_requests, Path}, State) ->
    request_throttler:enqueue(
        Path,
        #{
            credits => ?CREDITS
        },
        ?MODULE,
        State#state.data
    ),
    {noreply, State};
handle_cast({handle_response, Response}, State) ->
    ResponseBody = jsx:decode(Response#response.body, [return_maps]),
    #{~"choices" := Choices} = ResponseBody,
    lists:foreach(
        fun(#{~"message" := #{~"content" := Message}}) ->
            % TODO add ability to archive
            ?LOG_NOTICE(#{wrote_tmp_file => ?TMP_FILE, wrote_out_file => State#state.outfile}),
            strip_and_write(?TMP_FILE, Message),
            %
            os:cmd(?SOUND_COMMAND)
        end,
        Choices
    ),
    {noreply, State}.

test_os_cmd() ->
    Res = os:cmd(?SOUND_COMMAND),
    Res.


strip_and_write(Outfile, Message) ->
    Output = binary_to_list(Message),
    FilteredOutput = lists:filter(fun(C) -> C /= $` end, Output),
    file:write_file(Outfile, FilteredOutput).

handle_info(_Msg, State) ->
    {noreply, State}.
handle_call(Call, _From, _State) ->
    error({invalid_call, Call}).
