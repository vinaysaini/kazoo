%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Registration information and queries
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kapi_registration).

-export([bind_q/2, unbind_q/2
         ,declare_exchanges/0

         ,success/1, success/2
         ,success_v/1, success_v/2
         ,publish_success/1, publish_success/2
         ,success_keys/0

         ,flush/1, flush/2
         ,flush_v/1, flush_v/2
         ,publish_flush/1, publish_flush/2
        ]).

-include("kazoo_api.hrl").

-define(KEY_REG_SUCCESS, <<"registration.success">>).
-define(KEY_REG_QUERY, <<"registration.query">>).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    bind_q(Q, props:get_value('retrict_to', Props), Props).

bind_q(Q, 'undefined', Props) ->
    _ = amqp_util:bind_q_to_callmgr(Q, get_success_binding(Props)),
    amqp_util:bind_q_to_callmgr(Q, get_query_binding(Props));
bind_q(Q, ['reg_success'|T], Props) ->
    _ = amqp_util:bind_q_to_callmgr(Q, get_success_binding(Props)),
    bind_q(Q, T, Props);
bind_q(Q, ['reg_query'|T], Props) ->
    _ = amqp_util:bind_q_to_callmgr(Q, get_query_binding(Props)),
    bind_q(Q, T, Props);
bind_q(Q, ['reg_flush'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    _ = amqp_util:bind_q_to_callmgr(Q, get_flush_routing(Realm)),
    bind_q(Q, T, Props);
bind_q(Q, [_|T], Props) -> bind_q(Q, T, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    unbind_q(Q, props:get_value('retrict_to', Props), Props).

unbind_q(Q, 'undefined', Props) ->
    _ = amqp_util:unbind_q_from_callmgr(Q, get_success_binding(Props)),
    amqp_util:unbind_q_from_callmgr(Q, get_query_binding(Props));
unbind_q(Q, ['reg_success'|T], Props) ->
    _ = amqp_util:unbind_q_from_callmgr(Q, get_success_binding(Props)),
    unbind_q(Q, T, Props);
unbind_q(Q, ['reg_query'|T], Props) ->
    _ = amqp_util:unbind_q_from_callmgr(Q, get_query_binding(Props)),
    unbind_q(Q, T, Props);
unbind_q(Q, ['reg_flush'|T], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    _ = amqp_util:unbind_q_from_callmgr(Q, get_flush_routing(Realm)),
    unbind_q(Q, T, Props);
unbind_q(Q, [_|T], Props) -> unbind_q(Q, T, Props);
unbind_q(_, [], _) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callmgr_exchange().


-spec success(api_terms()) -> api_builder_resp().
-spec success(api_terms(), wh_proplist()) -> api_builder_resp().
success(API) -> success(API, []).

success(Props, Options) when is_list(Props) ->
    success(wh_json:from_list(Props), Options);
success(JObj, Options) ->
    case success_v(JObj, Options) of
        {'ok', FixedJObj} ->
            kz_api:build_message(FixedJObj);
        {'error', Errors} -> {'error', Errors};
        'false' -> {'error', "API failed validation for success"}
    end.

-spec success_v(api_terms()) -> api_validator_resp().
-spec success_v(api_terms(), wh_proplist()) -> api_validator_resp().
success_v(API) -> success_v(API, []).

success_v(Props, Options) when is_list(Props) ->
    success_v(wh_json:from_list(Props), Options);
success_v(JObj, Options) ->
    {'ok', Schema} = kz_api:find_schema(<<"reg_success">>),
    jesse:validate_with_schema(Schema, JObj, [{'allowed_errors', 2} | Options]).

publish_success(API) ->
    publish_success(API, ?DEFAULT_CONTENT_TYPE).
publish_success(API, ContentType) ->
    {'ok', Payload} = ?MODULE:success(API),
    amqp_util:callmgr_publish(Payload, ContentType, get_success_routing(API)).

%%--------------------------------------------------------------------
%% @doc Special access to the API keys
%% @end
%%--------------------------------------------------------------------
-spec success_keys() -> ne_binaries().
success_keys() ->
    DefaultKeys = wh_json:get_keys(?DEFAULT_HEADERS_SCHEMA),
    {'ok', Schema} = kz_api:find_schema(<<"reg_success">>),
    wh_json:get_keys(<<"properties">>, Schema) -- DefaultKeys.

-spec get_success_routing(api_terms()) -> ne_binary().
get_success_routing(Prop) when is_list(Prop) ->
    User = props:get_value(<<"Username">>, Prop),
    Realm = props:get_value(<<"Realm">>, Prop),
    get_success_routing(Realm, User);
get_success_routing(JObj) ->
    User = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    get_success_routing(Realm, User).

-spec get_success_routing(ne_binary(), ne_binary()) -> ne_binary().
get_success_routing(Realm, User) ->
    list_to_binary([?KEY_REG_SUCCESS
                    ,".", amqp_util:encode(Realm)
                    ,".", amqp_util:encode(User)
                   ]).

%% Allow Queues to be bound for specific realms, and even users within those realms
%% the resulting binding will be reg.success.{realm | *}.{user | *}
%% * matches one segment only, which means all success messages will be published to
%% "key.success.realm.user"
get_success_binding(Props) ->
    User = case props:get_value('user', Props) of
               'undefined' -> ".*";
               U -> [".", amqp_util:encode(U)]
           end,
    Realm = case props:get_value('realm', Props) of
                'undefined' -> ".*";
                R -> [".", amqp_util:encode(R)]
            end,

    iolist_to_binary([?KEY_REG_SUCCESS, Realm, User]).

get_query_binding(Props) ->
    User = case props:get_value('user', Props) of
               'undefined' -> ".*";
               U -> [".", amqp_util:encode(U)]
           end,
    Realm = case props:get_value('realm', Props) of
                'undefined' -> ".*";
                R -> [".", amqp_util:encode(R)]
            end,

    iolist_to_binary([?KEY_REG_QUERY, Realm, User]).

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec flush(api_terms()) -> api_builder_resp().
-spec flush(api_terms(), wh_proplist()) -> api_builder_resp().
flush(API) -> flush(API, []).

flush(Props, Options) when is_list(Props) ->
    flush(wh_json:from_list(Props), Options);
flush(JObj, Options) ->
    case flush_v(JObj, Options) of
        {'ok', FixedJObj} ->
            kz_api:build_message(FixedJObj);
        {'error', Errors} -> {'error', Errors};
        'false' -> {'error', "API failed validation for flush"}
    end.

-spec flush_v(api_terms()) -> api_validator_resp().
-spec flush_v(api_terms(), wh_proplist()) -> api_validator_resp().
flush_v(API) -> flush_v(API, []).

flush_v(Props, Options) when is_list(Props) ->
    flush_v(wh_json:from_list(Props), Options);
flush_v(JObj, Options) ->
    {'ok', Schema} = kz_api:find_schema(<<"reg_flush">>),
    jesse:validate_with_schema(Schema, JObj, [{'allowed_errors', 2} | Options]).

-spec publish_flush(api_terms()) -> 'ok'.
-spec publish_flush(api_terms(), ne_binary()) -> 'ok'.
publish_flush(API) ->
    publish_flush(API, ?DEFAULT_CONTENT_TYPE).
publish_flush(API, ContentType) ->
    {'ok', Payload} = ?MODULE:flush(API),
    amqp_util:callmgr_publish(Payload, ContentType, get_flush_routing(API)).

get_flush_routing(Realm) when is_binary(Realm) ->
    <<"registration.flush.", (amqp_util:encode(Realm))/binary>>;
get_flush_routing(Prop) when is_list(Prop) ->
    get_flush_routing(props:get_value(<<"Realm">>, Prop));
get_flush_routing(JObj) ->
    get_flush_routing(wh_json:get_value(<<"Realm">>, JObj)).

