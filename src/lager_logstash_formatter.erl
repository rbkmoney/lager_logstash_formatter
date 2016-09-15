%%% @doc Public API, supervisor and application startup.
%%% @end

-module(lager_logstash_formatter).

%% API
-export([format/2]).
-export([format/3]).


%%
%% API
%%
-spec format(lager_msg:lager_msg(), list(), _Colors) -> any().
format(Msg, Config, _Colors) ->
    format(Msg, Config).

-spec format(lager_msg:lager_msg(), list()) -> any().
format(Msg, Config) ->
    Json = jsx:encode(get_msg_map(Msg)),
    case proplists:get_value(output_as_list, Config) of
        undefined -> Json;
        _True     -> binary_to_list(Json)
    end.

get_msg_map(Msg) ->
    #{
        '@timestamp' => get_timestamp(Msg),
        '@severity'  => get_severity (Msg),
        'message'    => get_message  (Msg),
        '@metadata'  => get_metadata (Msg)
    }.

get_timestamp(Msg) ->
    {MegaSec, Sec, MicroSec} = lager_msg:timestamp(Msg),
    USec = MegaSec * 1000000000000 + Sec * 1000000 + MicroSec,
    {ok, TimeStamp} = rfc3339:format(USec, micro_seconds),
    TimeStamp.

get_severity(Msg) ->
    lager_msg:severity(Msg).

get_message(Msg) ->
    unicode:characters_to_binary(lager_msg:message(Msg), unicode).

get_metadata(Msg) ->
    case lager_msg:metadata(Msg) of
        []   -> [{}];
        Else -> lists:map(fun printable/1, Else)
    end.

%% can't naively encode `File` or `Pid` as json as jsx see them as lists
%% of integers
printable({file, File}) ->
    {file, unicode:characters_to_binary(File, unicode)};
printable({pid, Pid}) ->
    {pid, pid_list(Pid)};
%% if a value is expressable in json use it directly, otherwise
%% try to get a printable representation and express it as a json
%% string
printable({Key, Value}) when is_atom(Key); is_binary(Key) ->
    case jsx:is_term(Value) of
        true  -> {Key, Value};
        false -> {Key, unicode:characters_to_binary(io_lib:format("~p", [Value]), unicode)}
    end.

pid_list(Pid) ->
    try unicode:characters_to_binary(Pid, unicode) of
        Pid0 -> Pid0
    catch error:badarg ->
            unicode:characters_to_binary(hd(io_lib:format("~p", [Pid])), unicode)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% {{MegaSec, Sec, MicroSec}, <<"1970-01-01T00:00:00Z">>}
timestamp_now() ->
    Now = os:timestamp(),
    {MegaSec, Sec, MicroSec} = Now,
    {ok, TimeStamp} = rfc3339:format(
        MegaSec * 1000000000000 + Sec * 1000000 + MicroSec,
        micro_seconds
    ),
    {Now, TimeStamp}.

pid() ->
    Self = self(),
    {Self, unicode:characters_to_binary(io_lib:format("~p", [Self]), unicode)}.

format_test_() ->
    {Now, TimeStamp} = timestamp_now(),
    {Self, Pid} = pid(),
    [
        {"basic message", ?_assertEqual(
            <<"{\"@metadata\":{},\"@severity\":\"info\",\"@timestamp\":\"", TimeStamp/binary,
                "\",\"message\":\"hallo world\"}"
            >>,
            format(lager_msg:new("hallo world", Now, info, [], []), [])
        )},
        {"pid in metadata", ?_assertEqual(
            <<"{\"@metadata\":{\"pid\":\"", Pid/binary,
                "\"},\"@severity\":\"info\",\"@timestamp\":\"", TimeStamp/binary,
                "\",\"message\":\"hallo world\"}"
            >>,
            format(lager_msg:new("hallo world", Now, info, [{pid, io_lib:format("~p", [Self])}], []), [])
        )},
        {"bare pid in metadata", ?_assertEqual(
            <<"{\"@metadata\":{\"pid\":\"<0.6.0>\"},\"@severity\":\"info\",\"@timestamp\":\"", TimeStamp/binary,
                "\",\"message\":\"hallo world\"}"
            >>,
            format(lager_msg:new("hallo world", Now, info, [{pid, list_to_pid("<0.6.0>")}], []), [])
        )},
        {"file in metadata", ?_assertEqual(
            <<"{\"@metadata\":{\"file\":\"foo.erl\"},\"@severity\":\"info\",\"@timestamp\":\"", TimeStamp/binary,
                "\",\"message\":\"hallo world\"}"
            >>,
            format(lager_msg:new("hallo world", Now, info, [{file, "foo.erl"}], []), [])
        )},
        {"output as list", ?_assertEqual(
            "{\"@metadata\":{},\"@severity\":\"info\",\"@timestamp\":\"" ++ binary_to_list(TimeStamp) ++
                "\",\"message\":\"hallo world\"}",
            format(lager_msg:new("hallo world", Now, info, [], []), [{output_as_list, true}])
        )}
    ].

-endif.

