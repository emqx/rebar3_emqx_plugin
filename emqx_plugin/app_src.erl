-module({{name}}_plugin).

-include_lib("emqx/include/emqx.hrl").

-define(APP, {{name}}_plugin).

-export([register_metrics/0
  , load/0
  , unload/0
]).

-export([on_client_connected/4
  , on_client_disconnected/3
]).

-export([on_client_subscribe/3
  , on_client_unsubscribe/3
]).

-export([on_session_created/3
  , on_session_subscribed/4
  , on_session_unsubscribed/4
  , on_session_terminated/3
]).

-export([on_message_publish/2
  , on_message_deliver/3
  , on_message_acked/3
]).

-define(LOG(Level, Format, Args), emqx_logger:Level("{{name}}_plugin: " ++ Format, Args)).

register_metrics() ->
  [emqx_metrics:new(MetricName) || MetricName <- ['first_plugin.client_connected',
    'first_plugin.client_disconnected',
    'first_plugin.client_subscribe',
    'first_plugin.client_unsubscribe',
    'first_plugin.session_created',
    'first_plugin.session_subscribed',
    'first_plugin.session_unsubscribed',
    'first_plugin.session_terminated',
    'first_plugin.message_publish',
    'first_plugin.message_deliver',
    'first_plugin.message_acked']].

load() ->
  lists:foreach(
    fun({Hook, Fun, Filter}) ->
      load_(Hook, binary_to_atom(Fun, utf8), {Filter})
    end, parse_rule(application:get_env(?APP, rules, []))).

unload() ->
  lists:foreach(
    fun({Hook, Fun, _Filter}) ->
      unload_(Hook, binary_to_atom(Fun, utf8))
    end, parse_rule(application:get_env(?APP, rules, []))).


%%--------------------------------------------------------------------
%% Client connected
%%--------------------------------------------------------------------
on_client_connected(#{client_id := ClientId, username := Username}, 0, ConnInfo, _Env) ->
  emqx_metrics:inc('web_hook.client_connected'),
  %% Code Start

  %% Here is the code

  %% End
  ok;

on_client_connected(#{}, _ConnAck, _ConnInfo, _Env) ->
  ok.

%%--------------------------------------------------------------------
%% Client disconnected
%%--------------------------------------------------------------------
on_client_disconnected(#{}, auth_failure, _Env) ->
  ok;
on_client_disconnected(Client, {shutdown, Reason}, Env) when is_atom(Reason) ->
  on_client_disconnected(Reason, Client, Env);
on_client_disconnected(#{client_id := ClientId, username := Username}, Reason, _Env)
  when is_atom(Reason) ->
  emqx_metrics:inc('web_hook.client_disconnected'),
  %% Code Start

  %% Here is the code

  %% End
  ok;
on_client_disconnected(_, Reason, _Env) ->
  ?LOG(error, "Client disconnected, cannot encode reason: ~p", [Reason]),
  ok.

%%--------------------------------------------------------------------
%% Client subscribe
%%--------------------------------------------------------------------
on_client_subscribe(#{client_id := ClientId, username := Username}, TopicTable, {Filter}) ->
  lists:foreach(fun({Topic, Opts}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('web_hook.client_subscribe'),
        %% Code Start

        %% Here is the code

        %% End
        ok
      end, Topic, Filter)
                end, TopicTable).

%%--------------------------------------------------------------------
%% Client unsubscribe
%%--------------------------------------------------------------------
on_client_unsubscribe(#{client_id := ClientId, username := Username}, TopicTable, {Filter}) ->
  lists:foreach(fun({Topic, Opts}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('web_hook.client_unsubscribe'),
        %% Code Start

        %% Here is the code

        %% End
        ok
      end, Topic, Filter)
                end, TopicTable).

%%--------------------------------------------------------------------
%% Session created
%%--------------------------------------------------------------------
on_session_created(#{client_id := ClientId}, SessInfo, _Env) ->
  emqx_metrics:inc('web_hook.session_created'),
  %% Code Start

  %% Here is the code

  %% End
  ok.

%%--------------------------------------------------------------------
%% Session subscribed
%%--------------------------------------------------------------------
on_session_subscribed(#{client_id := ClientId}, Topic, Opts, {Filter}) ->
  with_filter(
    fun() ->
      emqx_metrics:inc('web_hook.session_subscribed'),
      %% Code Start

      %% Here is the code

      %% End
      ok
    end, Topic, Filter).

%%--------------------------------------------------------------------
%% Session unsubscribed
%%--------------------------------------------------------------------
on_session_unsubscribed(#{client_id := ClientId}, Topic, _Opts, {Filter}) ->
  with_filter(
    fun() ->
      emqx_metrics:inc('web_hook.session_unsubscribed'),
      %% Code Start

      %% Here is the code

      %% End
      ok
    end, Topic, Filter).

%%--------------------------------------------------------------------
%% Session terminated
%%--------------------------------------------------------------------
on_session_terminated(Info, {shutdown, Reason}, Env) when is_atom(Reason) ->
  on_session_terminated(Info, Reason, Env);
on_session_terminated(#{client_id := ClientId}, Reason, _Env) when is_atom(Reason) ->
  emqx_metrics:inc('web_hook.session_terminated'),
  %% Code Start

  %% Here is the code

  %% End
  ok;
on_session_terminated(#{}, Reason, _Env) ->
  ?LOG(error, "Session terminated, cannot encode the reason: ~p", [Reason]),
  ok.

%%--------------------------------------------------------------------
%% Message publish
%%--------------------------------------------------------------------
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};
on_message_publish(Message = #message{topic = Topic, flags = #{retain := Retain}}, {Filter}) ->
  with_filter(
    fun() ->
      emqx_metrics:inc('web_hook.message_publish'),
      %% Code Start

      %% Here is the code

      %% End
      {ok, Message}
    end, Message, Topic, Filter).

%%--------------------------------------------------------------------
%% Message deliver
%%--------------------------------------------------------------------
on_message_deliver(#{client_id := ClientId, username := Username},
    Message = #message{topic = Topic, flags = #{retain := Retain}},
    {Filter}) ->
  with_filter(
    fun() ->
      emqx_metrics:inc('web_hook.message_deliver'),
      %% Code Start

      %% Here is the code

      %% End
      {ok, Message}
    end, Topic, Filter).

%%--------------------------------------------------------------------
%% Message acked
%%--------------------------------------------------------------------
on_message_acked(#{client_id := ClientId}, Message = #message{topic = Topic, flags = #{retain := Retain}}, {Filter}) ->
  with_filter(
    fun() ->
      emqx_metrics:inc('web_hook.message_acked'),
      %% Code Start

      %% Here is the code

      %% End
      {ok, Message}
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
    true -> Fun(), ok;
    false -> ok
  end.

with_filter(Fun, _, _, undefined) ->
  Fun();
with_filter(Fun, Msg, Topic, Filter) ->
  case emqx_topic:match(Topic, Filter) of
    true -> Fun();
    false -> {ok, Msg}
  end.

a2b(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
a2b(A) -> A.

load_(Hook, Fun, Params) ->
  case Hook of
    'client.connected' -> emqx:hook(Hook, fun ?MODULE:Fun/4, [Params]);
    'client.disconnected' -> emqx:hook(Hook, fun ?MODULE:Fun/3, [Params]);
    'client.subscribe' -> emqx:hook(Hook, fun ?MODULE:Fun/3, [Params]);
    'client.unsubscribe' -> emqx:hook(Hook, fun ?MODULE:Fun/3, [Params]);
    'session.created' -> emqx:hook(Hook, fun ?MODULE:Fun/3, [Params]);
    'session.subscribed' -> emqx:hook(Hook, fun ?MODULE:Fun/4, [Params]);
    'session.unsubscribed' -> emqx:hook(Hook, fun ?MODULE:Fun/4, [Params]);
    'session.terminated' -> emqx:hook(Hook, fun ?MODULE:Fun/3, [Params]);
    'message.publish' -> emqx:hook(Hook, fun ?MODULE:Fun/2, [Params]);
    'message.acked' -> emqx:hook(Hook, fun ?MODULE:Fun/3, [Params]);
    'message.deliver' -> emqx:hook(Hook, fun ?MODULE:Fun/3, [Params])
  end.

unload_(Hook, Fun) ->
  case Hook of
    'client.connected' -> emqx:unhook(Hook, fun ?MODULE:Fun/4);
    'client.disconnected' -> emqx:unhook(Hook, fun ?MODULE:Fun/3);
    'client.subscribe' -> emqx:unhook(Hook, fun ?MODULE:Fun/3);
    'client.unsubscribe' -> emqx:unhook(Hook, fun ?MODULE:Fun/3);
    'session.created' -> emqx:unhook(Hook, fun ?MODULE:Fun/3);
    'session.subscribed' -> emqx:unhook(Hook, fun ?MODULE:Fun/4);
    'session.unsubscribed' -> emqx:unhook(Hook, fun ?MODULE:Fun/4);
    'session.terminated' -> emqx:unhook(Hook, fun ?MODULE:Fun/3);
    'message.publish' -> emqx:unhook(Hook, fun ?MODULE:Fun/2);
    'message.acked' -> emqx:unhook(Hook, fun ?MODULE:Fun/3);
    'message.deliver' -> emqx:unhook(Hook, fun ?MODULE:Fun/3)
  end.
