-module(util).

-export([
    get_nested/2,
    maybe_zeropadded/1,
    now_timestamp/0,
    to_iso/1,
    config_to_tuples/1,
    find/2
]).

find(P, XS) ->
    case lists:filter(P, XS) of
        [X | _] -> X;
        [] -> null
    end.

get_nested([Key], Map) -> maps:get(Key, Map);
get_nested([Key | Keys], Map) -> get_nested(Keys, maps:get(Key, Map)).

% Pad at most 0 zeroes to and make it a string
maybe_zeropadded(Number) when Number < 10 -> lists:concat(["0", integer_to_list(Number)]);
maybe_zeropadded(Number) -> integer_to_list(Number).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Time

now_timestamp() ->
    {Mega, Secs, Micro} = os:timestamp(),
    calendar:now_to_universal_time({Mega, Secs, Micro}).

to_iso(Ts) ->
    case Ts of
        {{{YY, MM, DD}, {HH, MI, SS}}, MSec} ->
            io_lib:format(
                "~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                [YY, MM, DD, HH, MI, SS, MSec]
            );
        {{YY, MM, DD}, {HH, MI, SS}} ->
            io_lib:format(
                "~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                [YY, MM, DD, HH, MI, SS]
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Config

config_to_tuples(List) ->
    lists:map(
        fun(#{<<"symbol">> := Symbol, <<"currency">> := Currency}) ->
            {Symbol, binary_to_atom(Currency)}
        end,
        lists:nth(1, lists:map(fun(Elem) -> maps:values(Elem) end, List))
    ).
