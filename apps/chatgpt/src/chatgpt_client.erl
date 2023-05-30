-include_lib("include/logger.hrl").

-module(chatgpt_client).

-export([start_link/2, handle_response/2]).

-behaviour(gen_server).

-define(CREDITS, 1).

-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3
]).

-record(state, {
    redis_key :: binary(),
    redis_conn :: pid(),
    data :: any()
}).

-record(response, {
    meta :: map(),
    body :: map()
}).

start_link(RedisKey, Data) ->
    gen_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        #{redis_key => RedisKey, data => Data},
        []
    ).

init(#{redis_key := RedisKey, data := Data}) ->
    {ok, RedisConn} = eredis:start_link(),
    do_requests(RedisKey),
    State = #state{
        redis_key = RedisKey,
        redis_conn = RedisConn,
        data = Data
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
    #{<<"choices">> := Choices} = ResponseBody,
    lists:foreach(
        fun(#{<<"message">> := #{<<"content">> := Message}}) ->
            ?LOG_NOTICE(#{message => binary_to_list(Message)})
        end,
        Choices
    ),
    % lists:map(fun(Stock) -> write_redis(map_response(Stock), State) end, ResponseBody),
    {noreply, State}.

write_redis(#{'_symbol' := Symbol} = Dto, #state{redis_key = Key, redis_conn = Conn}) ->
    {ok, _Result} = eredis:q(Conn, [
        "SET",
        iolist_to_binary([Key, <<"/">>, Symbol]),
        jsx:encode(Dto)
    ]).

map_response(#{
    <<"symbol">> := Symbol,
    <<"name">> := Name,
    <<"currency">> := Currency,
    <<"exchange">> := Exchange,
    <<"country">> := Country
}) ->
    #{
        '_symbol' => Symbol,
        '_name' => Name,
        '_currency' => Currency,
        '_exchange' => Exchange,
        '_country' => Country
    }.

handle_info(_Msg, State) ->
    {noreply, State}.
handle_call(Call, _From, _State) ->
    error({invalid_call, Call}).
