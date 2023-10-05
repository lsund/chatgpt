-include_lib("include/logger.hrl").

-module(request_throttler).

-behaviour(gen_server).

-export([start_link/3, enqueue/4]).

-export([
    init/1,
    handle_info/2,
    handle_cast/2,
    handle_call/3
]).

-define(TIMEOUT, 2 * 60000).

-record(state, {queue :: list(), conn :: pid(), api_key :: binary()}).

start_link(Host, ApiKey, Rate) ->
    gen_server:start_link(
        {local, request_throttler},
        ?MODULE,
        #{host => Host, api_key => ApiKey, rate => Rate},
        []
    ).

init(#{host := Host, api_key := ApiKey, rate := Rate}) ->
    {ok, Conn} = gun:open(
        binary_to_list(Host),
        443,
        #{
            connect_timeout => timer:seconds(15),
            retry => 3,
            retry_timeout => timer:seconds(30),
            tls_opts => [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}]
        }
    ),
    ?LOG_NOTICE(#{msg => connection_established, host => binary_to_list(Host)}),
    Minute = 1000 * 60,
    timer:send_interval(round(Minute / Rate), ping),
    {ok, #state{queue = [], conn = Conn, api_key = ApiKey}}.

enqueue(Path, Meta, Module, Data) ->
    ?LOG_NOTICE(#{msg => enqueue, path => Path}),
    gen_server:cast(?MODULE, {add, {Path, Meta, Data}, Module}),
    ok.

handle_cast({add, {Path, Meta, Data}, Module}, State) ->
    URI = uri_string:recompose(#{
        path => Path
    }),
    Request = {URI, Module, Meta, Data},
    {noreply, State#state{
        queue = [
            Request | State#state.queue
        ]
    }}.

handle_info(ping, State) ->
    Queue1 =
        case State#state.queue of
            [Request | TL] ->
                case continue(State#state.conn, State#state.api_key, Request) of
                    done -> TL;
                    Request1 -> [Request1 | TL]
                end;
            [] ->
                []
        end,
    {noreply, State#state{queue = Queue1}};
handle_info(_Msg, State) ->
    {noreply, State}.

continue(Conn, ApiKey, {URI, Module, Meta, Data} = Request) ->
    Credits = maps:get(credits, Meta),
    case Credits - 1 of
        0 ->
            do_rest_call(Conn, ApiKey, Request),
            done;
        _ ->
            {URI, Module, maps:update(credits, Credits - 1, Meta, Data)}
    end.

do_rest_call(Conn, ApiKey, {URI, Module, Meta, Data}) ->
    Headers = #{
        <<"Content-type">> => <<"application/json">>,
        <<"Authorization">> =>
            iolist_to_binary([<<"Bearer ">>, ApiKey])
    },
    RequestBody = jsx:encode(Data),
    StreamRef = gun:request(Conn, <<"POST">>, URI, Headers, RequestBody),
    case gun:await(Conn, StreamRef, ?TIMEOUT) of
        {response, nofin, 200, _Headers} ->
            ok;
        Error ->
            ?LOG_ERROR(#{gun_failed => Error, part => headers}),
            {error, Error}
    end,
    case gun:await_body(Conn, StreamRef, ?TIMEOUT) of
        {ok, ResponseBody} ->
            Module:handle_response(Meta, ResponseBody);
        Error2 ->
            ?LOG_ERROR(#{gun_failed => Error2, part => body}),
            {error, Error2}
    end.

handle_call(Call, _From, _State) ->
    error({invalid_cast, Call}).
