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
    Filters = proplists:get_value(filters, Config, []),
    FilteredMsg = filter(get_msg_map(Msg), Filters),
    [jsx:encode(FilteredMsg), <<"\n">>].

get_msg_map(Msg) ->
    maps:merge(
        get_metadata(Msg),
        #{
            '@timestamp' => get_timestamp(Msg),
            '@severity'  => get_severity (Msg),
            'message'    => get_message  (Msg)
         }
   ).

-spec get_timestamp(lager_msg:lager_msg()) -> binary().
get_timestamp(Msg) ->
    {MegaSec, Sec, MicroSec} = lager_msg:timestamp(Msg),
    USec = MegaSec * 1000000000000 + Sec * 1000000 + MicroSec,
    {ok, TimeStamp} = rfc3339:format(USec, micro_seconds),
    TimeStamp.

-spec get_severity(lager_msg:lager_msg()) -> atom().
get_severity(Msg) ->
    lager_msg:severity(Msg).

-spec get_message(lager_msg:lager_msg()) -> binary().
get_message(Msg) ->
    unicode:characters_to_binary(lager_msg:message(Msg), unicode).

-spec get_metadata(lager_msg:lager_msg()) -> map().
get_metadata(Msg) ->
    case lager_msg:metadata(Msg) of
        []   -> #{};
        Else -> lists:foldl(fun add_meta/2, #{}, Else)
    end.

add_meta(MetaItem, Map) ->
    {Key, Value} = printable(MetaItem),
    Map#{Key => Value}.

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

%%filters
filter(#{'message' := Message} = Msg, Filters) ->
    Msg#{'message' => lists:foldl(fun apply_filter/2, Message, Filters)}.

apply_filter(Filter, Message) ->
    unicode:characters_to_binary(re:replace(Message, compiled_filter(Filter), "[***]"), unicode).

compiled_filter(Filter) ->
    case application:get_env(?MODULE, compiled_filters, #{}) of
        #{Filter := CompiledFilter} ->
            CompiledFilter;
        #{} = Filters ->
            {ok, CompiledFilter} = re:compile(Filter, [unicode]),
            application:set_env(?MODULE, compiled_filters, Filters#{Filter => CompiledFilter}),
            CompiledFilter
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
        )},
        {"filtered message", ?_assertEqual(
            [<<"{\"@severity\":\"info\",\"@timestamp\":\"",
               TimeStamp/binary, "\",\"message\":\"one [***] three [***]\"}">>, <<"\n">>],
            format(lager_msg:new("one two three four", Now, info, [], []), [{filters, ["two", "four"]}])
        )}
    ].

-endif.

