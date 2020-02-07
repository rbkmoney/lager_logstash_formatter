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
format(Msg, _Config) ->
    [jsx:encode(get_msg_meta(Msg)), <<"\n">>].

get_msg_meta(Msg) ->
    lists:merge(
        get_metadata(Msg),
        [
            {'@severity', get_severity(Msg)},
            {'@timestamp', get_timestamp(Msg)},
            {'message', get_message(Msg)}
         ]
   ).

-spec get_timestamp(lager_msg:lager_msg()) -> binary().
get_timestamp(Msg) ->
    {MegaSec, Sec, MicroSec} = lager_msg:timestamp(Msg),
    USec = MegaSec * 1000000000000 + Sec * 1000000 + MicroSec,
    {ok, TimeStamp} = time_format(USec),
    TimeStamp.

-spec get_severity(lager_msg:lager_msg()) -> atom().
get_severity(Msg) ->
    lager_msg:severity(Msg).

-spec get_message(lager_msg:lager_msg()) -> list().
get_message(Msg) ->
    list_to_binary(lager_msg:message(Msg)).

-spec get_metadata(lager_msg:lager_msg()) -> list().
get_metadata(Msg) ->
    case lager_msg:metadata(Msg) of
        []   -> [];
        Else -> lists:foldl(fun add_meta/2, [], Else)
    end.

add_meta(MetaItem, Acc) ->
    {Key, Value} = printable(MetaItem),
    [{Key, Value} | Acc].

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

time_format(Microsec) ->
    {Year, Month, Day, Hour, Min, Sec, USec} = convert(Microsec),
    FormattedDate = format_date(Year, Month, Day),
    FormattedTime = format_time(Hour, Min, Sec, USec),
    {ok, format_(FormattedDate, FormattedTime)}.

format_date(Y, M, D) when is_integer(Y), is_integer(M), is_integer(D) ->
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Y, M, D]);
format_date(_, _, _) -> {error, baddate}.

format_time(H, M, S, 0) when is_integer(H), is_integer(M), is_integer(S) ->
    io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [H, M, S]);
format_time(H, M, S, U) when is_integer(H), is_integer(M), is_integer(S), is_integer(U) ->
    SU = (S / 1) + (U / 1000000),
    io_lib:format("~2.10.0B:~2.10.0B:~9.6.0f", [H, M, SU]);
format_time(_, _, _, _) -> {error, badtime}.

format_({error, baddate}, _) -> {error, badarg};
format_(_, {error, badtime}) -> {error, badtime};
format_(Date, Time) -> unicode:characters_to_binary([Date, "T", Time]).

convert(Time) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    GregorianSeconds = Time div 1000000 + Epoch,
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    USec = Time rem 1000000,
    {Year, Month, Day, Hour, Min, Sec, USec}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% {{MegaSec, Sec, MicroSec}, <<"1970-01-01T00:00:00Z">>}
timestamp_now() ->
    Now = os:timestamp(),
    {MegaSec, Sec, MicroSec} = Now,
    {ok, TimeStamp} = time_format(MegaSec * 1000000000000 + Sec * 1000000 + MicroSec),
    {Now, TimeStamp}.

pid() ->
    Self = self(),
    {Self, unicode:characters_to_binary(io_lib:format("~p", [Self]), unicode)}.

format_test_() ->
    {Now, TimeStamp} = timestamp_now(),
    {Self, Pid} = pid(),
    [
        {"basic message", ?_assertEqual(
            [<<"{\"@severity\":\"info\",\"@timestamp\":\"",
               TimeStamp/binary, "\",\"message\":\"hallo world\"}">>, <<"\n">>],
            format(lager_msg:new("hallo world", Now, info, [], []), [])
        )},
        {"pid in metadata", ?_assertEqual(
            [<<"{\"@severity\":\"info\",\"@timestamp\":\"", TimeStamp/binary,
               "\",\"message\":\"hallo world\",\"pid\":\"", Pid/binary, "\"}">>, <<"\n">>],
            format(lager_msg:new("hallo world", Now, info, [{pid, io_lib:format("~p", [Self])}], []), [])
        )},
        {"bare pid in metadata", ?_assertEqual(
            [<<"{\"@severity\":\"info\",\"@timestamp\":\"", TimeStamp/binary,
               "\",\"message\":\"hallo world\",\"pid\":\"<0.6.0>\"}">>, <<"\n">>],
            format(lager_msg:new("hallo world", Now, info, [{pid, list_to_pid("<0.6.0>")}], []), [])
        )},
        {"file in metadata", ?_assertEqual(
            [<<"{\"@severity\":\"info\",\"@timestamp\":\"", TimeStamp/binary,
               "\",\"file\":\"foo.erl\",\"message\":\"hallo world\"}">>, <<"\n">>],
            format(lager_msg:new("hallo world", Now, info, [{file, "foo.erl"}], []), [])
        )}
    ].

-endif.

