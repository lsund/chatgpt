-module(chatgpt_sup).

-behaviour(supervisor).

-include_lib("include/logger.hrl").

-export([start_link/0, restart_and_query/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

restart_and_query() ->
    Data = reload_config(),
    [
        #{
            <<"redis">> := #{
                <<"key">> := #{
                    <<"models">> := RedisKey
                }
            }
        }
    ] = yamerl:decode_file("config/simple.yaml", [
        str_node_as_binary, {map_node_format, map}
    ]),
    ok = supervisor:terminate_child(?MODULE, client),
    ok = supervisor:delete_child(?MODULE, client),
    supervisor:start_child(?MODULE, #{
        id => client, start => {chatgpt_client, start_link, [RedisKey, Data]}
    }).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    [
        #{
            <<"requests_per_minute">> := Rate,
            <<"openai">> := #{
                <<"api_key">> := ApiKey,
                <<"host">> := Host
            },
            <<"redis">> := #{
                <<"key">> := #{
                    <<"models">> := RedisKey
                }
            }
        }
    ] = yamerl:decode_file("config/simple.yaml", [
        str_node_as_binary, {map_node_format, map}
    ]),
    [Data] = yamerl:decode_file("config/data.yaml", [
        str_node_as_binary, {map_node_format, map}
    ]),
    ChildSpecs = [
        #{
            id => throttler,
            start =>
                {request_throttler, start_link, [Host, ApiKey, Rate]}
        },
        #{
            id => client,
            start =>
                {chatgpt_client, start_link, [RedisKey, Data]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

reload_config() ->
    [Data] = yamerl:decode_file("config/data.yaml", [
        str_node_as_binary, {map_node_format, map}
    ]),
    Data.
