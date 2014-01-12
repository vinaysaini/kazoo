%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Route requests, responses, and wins!
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kapi_configuration).

-export([doc_update/1, doc_update_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_doc_update/1, publish_doc_update/2
         ,api_doc/1
         ,api_db/1
         ,api_id/1
         ,api_type/1
         ,api_action/1
        ]).

-include("kazoo_api.hrl").
%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for authn gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q(binary(), wh_proplist()) -> 'ok'.
bind_q(Q, Props) ->
    RoutingKey = get_routing_key(Props),
    amqp_util:bind_q_to_configuration(Q, RoutingKey).

-spec unbind_q(binary(), wh_proplist()) -> 'ok'.
unbind_q(Q, Props) ->
    RoutingKey = get_routing_key(Props),
    amqp_util:unbind_q_from_configuration(Q, RoutingKey).

-spec get_routing_key(wh_proplist()) -> binary().
get_routing_key(Props) ->
    Action = props:get_binary_value('action', Props, <<"*">>),
    Db = props:get_binary_value('db', Props, <<"*">>),
    Type = props:get_first_defined(['doc_type', 'type'], Props, <<"*">>),
    Id = props:get_first_defined(['doc_id', 'id'], Props, <<"*">>),
    amqp_util:document_routing_key(Action, Db, Type, Id).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:configuration_exchange().

-spec doc_update(api_terms()) -> api_builder_resp().
-spec doc_update(api_terms(), wh_proplist()) -> api_builder_resp().
doc_update(API) -> doc_update(API, []).

doc_update(Props, Options) when is_list(Props) ->
    doc_update(wh_json:from_list(Props), Options);
doc_update(JObj, Options) ->
    case doc_update_v(JObj, Options) of
        {'ok', FixedJObj} ->
            kz_api:build_message(FixedJObj);
        {'error', Errors} -> {'error', Errors};
        'false' -> {'error', "API failed validation for doc_update"}
    end.

-spec doc_update_v(api_terms()) -> api_validator_resp().
-spec doc_update_v(api_terms(), wh_proplist()) -> api_validator_resp().
doc_update_v(API) -> doc_update_v(API, []).

doc_update_v(Props, Options) when is_list(Props) ->
    doc_update_v(wh_json:from_list(Props), Options);
doc_update_v(JObj, Options) ->
    {'ok', Schema} = kz_api:find_schema(<<"config_update">>),
    jesse:validate_with_schema(Schema, JObj, [{'allowed_errors', 2} | Options]).

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_doc_update(api_terms()) -> 'ok'.
-spec publish_doc_update(api_terms(), binary()) -> 'ok'.
publish_doc_update(JObj) ->
    publish_doc_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_doc_update(API, ContentType) ->
    {'ok', Payload} = ?MODULE:doc_update(API),
    amqp_util:document_change_publish(api_action(API)
                                      ,api_db(API)
                                      ,api_type(API)
                                      ,api_id(API)
                                      ,Payload
                                      ,ContentType
                                     ).

api_action(API) when is_list(API) -> props:get_value(<<"Event-Name">>, API);
api_action(API) -> api_action(wh_json:to_proplist(API)).

api_db(API) when is_list(API) -> props:get_value(<<"Database">>, API);
api_db(API) -> api_db(wh_json:to_proplist(API)).

api_type(API) when is_list(API) -> props:get_value(<<"Type">>, API);
api_type(API) -> api_type(wh_json:to_proplist(API)).

api_id(API) when is_list(API) -> props:get_value(<<"ID">>, API);
api_id(API) -> api_id(wh_json:to_proplist(API)).

api_doc(API) when is_list(API) -> props:get_value(<<"Doc">>, API);
api_doc(API) -> api_doc(wh_json:to_proplist(API)).
