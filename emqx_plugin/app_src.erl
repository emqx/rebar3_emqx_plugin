-module({{name}}).

-include("{{name}}.hrl").
-include_lib("emqx/include/emqx.hrl").

-export([ register_metrics/0
        , load/0
        , unload/0
        ]).

-export([ on_client_connected/3
        , on_client_disconnected/4
        ]).

-export([ on_client_subscribe/4
        , on_client_unsubscribe/4
        ]).

-export([ on_session_subscribed/4
        , on_session_unsubscribed/4
        ]).

-export([ on_message_publish/2
        , on_message_delivered/3
        , on_message_acked/3
        ]).

-define(LOG(Level, Format, Args), emqx_logger:Level("{{name}}: " ++ Format, Args)).

register_metrics() ->
    [emqx_metrics:new(MetricName) || MetricName <- ['{{name}}.client_connected',
                                                    '{{name}}.client_disconnected',
                                                    '{{name}}.client_subscribe',
                                                    '{{name}}.client_unsubscribe',
                                                    '{{name}}.session_subscribed',
                                                    '{{name}}.session_unsubscribed',
                                                    '{{name}}.message_publish',
                                                    '{{name}}.message_delivered',
                                                    '{{name}}.message_acked']].

load() ->
    lists:foreach(
      fun({Hook, Fun, Filter}) ->
          load_(Hook, binary_to_atom(Fun, utf8), {Filter})
      end, parse_rule(application:get_env(?APP, hooks, []))).

unload() ->
    lists:foreach(
      fun({Hook, Fun, _Filter}) ->
          unload_(Hook, binary_to_atom(Fun, utf8))
      end, parse_rule(application:get_env(?APP, hooks, []))).


%%--------------------------------------------------------------------
%% Client connected
%%--------------------------------------------------------------------
on_client_connected(#{clientid := ClientId, username := Username}, ConnInfo, _Env) ->
    emqx_metrics:inc('{{name}}.client_connected'),
    %% Code Start

    %% Here is the code

    %% End
    ok;

on_client_connected(#{}, _ConnInfo, _Env) ->
    ok.

%%--------------------------------------------------------------------
%% Client disconnected
%%--------------------------------------------------------------------
on_client_disconnected(#{}, auth_failure, _ConnInfo, _Env) ->
    ok;
on_client_disconnected(Client, {shutdown, Reason}, ConnInfo, Env) when is_atom(Reason) ->
    on_client_disconnected(Client, Reason, ConnInfo, Env);
on_client_disconnected(#{clientid := ClientId, username := Username}, Reason, _ConnInfo, _Env)
    when is_atom(Reason) ->
    emqx_metrics:inc('{{name}}.client_disconnected'),
    %% Code Start

    %% Here is the code

    %% End
    ok;
on_client_disconnected(_, Reason, _ConnInfo, _Env) ->
    ?LOG(error, "Client disconnected, cannot encode reason: ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Client subscribe
%%--------------------------------------------------------------------
on_client_subscribe(#{clientid := ClientId, username := Username}, Properties, RawTopicFilters, {Filter}) ->
    lists:foreach(fun({Topic, Opts}) ->
      with_filter(
        fun() ->
          emqx_metrics:inc('{{name}}.client_subscribe'),
          %% Code Start

          %% Here is the code

          %% End
          ok
        end, Topic, Filter)
    end, RawTopicFilters).

%%--------------------------------------------------------------------
%% Client unsubscribe
%%--------------------------------------------------------------------
on_client_unsubscribe(#{clientid := ClientId, username := Username}, Properties, RawTopicFilters, {Filter}) ->
    lists:foreach(fun({Topic, Opts}) ->
      with_filter(
        fun() ->
          emqx_metrics:inc('{{name}}.client_unsubscribe'),
          %% Code Start

          %% Here is the code

          %% End
          ok
        end, Topic, Filter)
    end, RawTopicFilters).

%%--------------------------------------------------------------------
%% Session subscribed
%%--------------------------------------------------------------------
on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, {Filter}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('{{name}}.session_subscribed'),
        %% Code Start

        %% Here is the code

        %% End
        ok
      end, Topic, Filter).

%%--------------------------------------------------------------------
%% Session unsubscribed
%%--------------------------------------------------------------------
on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, {Filter}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('{{name}}.session_unsubscribed'),
        %% Code Start

        %% Here is the code

        %% End
        ok
      end, Topic, Filter).

%%--------------------------------------------------------------------
%% Message publish
%%--------------------------------------------------------------------
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};
on_message_publish(Message = #message{topic = Topic, flags = #{retain := Retain}}, {Filter}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('{{name}}.message_publish'),
        %% Code Start

        %% Here is the code

        %% End
        {ok, Message}
      end, Message, Topic, Filter).

%%--------------------------------------------------------------------
%% Message deliver
%%--------------------------------------------------------------------
on_message_delivered(#{clientid := ClientId, username := Username},
                   Message = #message{topic = Topic, payload = Payload},
                   {Filter}) ->
  with_filter(
    fun() ->
      emqx_metrics:inc('{{name}}.message_delivered'),
      %% Code Start

      %% Here is the code

      %% End
      ok
    end, Topic, Filter).

%%--------------------------------------------------------------------
%% Message acked
%%--------------------------------------------------------------------
on_message_acked(#{clientid := ClientId}, Message = #message{topic = Topic}, {Filter}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('{{name}}.message_acked'),
        %% Code Start

        %% Here is the code

        %% End
        ok
      end, Topic, Filter).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
parse_rule(Rules) ->
    parse_rule(Rules, []).
parse_rule([], Acc) ->
    lists:reverse(Acc);
parse_rule([{Rule, Conf} | Rules], Acc) ->
    Params = jsx:decode(iolist_to_binary(Conf)),
    Action = proplists:get_value(<<"action">>, Params),
    Filter = proplists:get_value(<<"topic">>, Params),
    parse_rule(Rules, [{list_to_atom(Rule), Action, Filter} | Acc]).

with_filter(Fun, _, undefined) ->
    Fun(), ok;
with_filter(Fun, Topic, Filter) ->
    case emqx_topic:match(Topic, Filter) of
        true  -> Fun(), ok;
        false -> ok
    end.

with_filter(Fun, _, _, undefined) ->
    Fun();
with_filter(Fun, Msg, Topic, Filter) ->
    case emqx_topic:match(Topic, Filter) of
        true  -> Fun();
        false -> {ok, Msg}
    end.

load_(Hook, Fun, Params) ->
    case Hook of
        'client.connected'    -> emqx:hook(Hook, fun ?MODULE:Fun/4, [Params]);
        'client.disconnected' -> emqx:hook(Hook, fun ?MODULE:Fun/4, [Params]);
        'client.subscribe'    -> emqx:hook(Hook, fun ?MODULE:Fun/4, [Params]);
        'client.unsubscribe'  -> emqx:hook(Hook, fun ?MODULE:Fun/4, [Params]);
        'session.subscribed'  -> emqx:hook(Hook, fun ?MODULE:Fun/4, [Params]);
        'session.unsubscribed'-> emqx:hook(Hook, fun ?MODULE:Fun/4, [Params]);
        'message.publish'     -> emqx:hook(Hook, fun ?MODULE:Fun/2, [Params]);
        'message.acked'       -> emqx:hook(Hook, fun ?MODULE:Fun/3, [Params]);
        'message.delivered'   -> emqx:hook(Hook, fun ?MODULE:Fun/3, [Params])
    end.

unload_(Hook, Fun) ->
    case Hook of
        'client.connected'    -> emqx:unhook(Hook, fun ?MODULE:Fun/4);
        'client.disconnected' -> emqx:unhook(Hook, fun ?MODULE:Fun/4);
        'client.subscribe'    -> emqx:unhook(Hook, fun ?MODULE:Fun/4);
        'client.unsubscribe'  -> emqx:unhook(Hook, fun ?MODULE:Fun/4);
        'session.subscribed'  -> emqx:unhook(Hook, fun ?MODULE:Fun/4);
        'session.unsubscribed'-> emqx:unhook(Hook, fun ?MODULE:Fun/4);
        'message.publish'     -> emqx:unhook(Hook, fun ?MODULE:Fun/2);
        'message.acked'       -> emqx:unhook(Hook, fun ?MODULE:Fun/3);
        'message.delivered'   -> emqx:unhook(Hook, fun ?MODULE:Fun/3)
    end.

