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
    Regexes = proplists:get_value(message_redaction_regex_list, Config, []),
    RedactedMsg = redact(get_msg_map(Msg), Regexes),
    [jsx:encode(RedactedMsg), <<"\n">>].

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
    lager_msg:message(Msg).

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
redact(#{'message' := Message} = Msg, Regexes) ->
    Msg#{'message' => redact_all(unicode:characters_to_binary(Message, unicode), Regexes)}.

redact_all(Message, Regexes) ->
    lists:foldl(fun redact_one/2, Message, Regexes).

redact_one(Regex, Message) ->
    case re:run(Message, compile_regex(Regex), [global, {capture, first, index}]) of
        {match, Captures} ->
            lists:foldl(fun redact_capture/2, Message, Captures);
        nomatch ->
            Message
    end.

redact_capture({S, Len}, Message) ->
    <<Pre:S/binary, _:Len/binary, Rest/binary>> = Message,
    <<Pre/binary, (binary:copy(<<"*">>, Len))/binary, Rest/binary>>;
redact_capture([Capture], Message) ->
    redact_capture(Capture, Message).

compile_regex(Regex) ->
    case application:get_env(?MODULE, message_redaction_compiled_regexes, #{}) of
        #{Regex := CompiledRegex} ->
            CompiledRegex;
        #{} = CompiledRegexes ->
            {ok, CompiledRegex} = re:compile(Regex, [unicode]),
            ok = application:set_env(
                ?MODULE,
                message_redaction_compiled_regexes,
                CompiledRegexes#{Regex => CompiledRegex}
            ),
            CompiledRegex
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
               TimeStamp/binary, "\",\"message\":\"one *** three four ******\"}">>, <<"\n">>],
            format(
                lager_msg:new("one two three four murder", Now, info, [], []),
                [{message_redaction_regex_list, ["two", "[d-u]{6}"]}]
            )
        )},
        {"filtered message", ?_assertEqual(
            [<<"{\"@severity\":\"info\",\"@timestamp\":\"",
               TimeStamp/binary, "\",\"message\":\"*** *** **** ***\"}">>, <<"\n">>],
            format(
                lager_msg:new("two two four two", Now, info, [], []),
                [{message_redaction_regex_list, ["two", "four"]}]
            )
        )}
    ].

-endif.

