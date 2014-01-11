%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Route requests, responses, and wins!
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kapi_route).

-export([req/1, req_v/1
         ,resp/1, resp_v/1
         ,win/1, win_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_req/1, publish_req/2
         ,publish_resp/2, publish_resp/3
         ,publish_win/2, publish_win/3
         ,get_auth_realm/1
         ,get_auth_user/1
         ,is_actionable_resp/1
        ]).

-include("kazoo_api.hrl").

 %% corresponds to the route_req/1 api call
-define(KEY_ROUTE_REQ, <<"route.req">>).

%%--------------------------------------------------------------------
%% @doc Setup and tear down bindings for authn gen_listeners
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    User = props:get_value('user', Props, <<"*">>),
    amqp_util:bind_q_to_callmgr(Queue, get_route_req_routing(Realm, User)).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    User = props:get_value('user', Props, <<"*">>),
    amqp_util:unbind_q_from_callmgr(Queue, get_route_req_routing(Realm, User)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callmgr_exchange().

-spec req(api_terms()) -> api_builder_resp().
-spec req(api_terms(), wh_proplist()) -> api_builder_resp().
req(API) -> req(API, []).

req(Props, Options) when is_list(Props) ->
    req(wh_json:from_list(Props), Options);
req(JObj, Options) ->
    case req_v(JObj, Options) of
        {'ok', FixedJObj} ->
            kz_api:build_message(FixedJObj);
        {'error', Errors} -> {'error', Errors};
        'false' -> {'error', "API failed validation for route_req"}
    end.

-spec req_v(api_terms()) -> api_validator_resp().
-spec req_v(api_terms(), wh_proplist()) -> api_validator_resp().
req_v(API) -> req_v(API, []).

req_v(Props, Options) when is_list(Props) ->
    req_v(wh_json:from_list(Props), Options);
req_v(JObj, Options) ->
    {'ok', Schema} = kz_api:find_schema(<<"route_req">>),
    jesse:validate_with_schema(Schema, JObj, [{'allowed_errors', 2} | Options]).

-spec resp(api_terms()) -> api_builder_resp().
-spec resp(api_terms(), wh_proplist()) -> api_builder_resp().
resp(API) -> resp(API, []).

resp(Props, Options) when is_list(Props) ->
    resp(wh_json:from_list(Props), Options);
resp(JObj, Options) ->
    case resp_v(JObj, Options) of
        {'ok', FixedJObj} ->
            kz_api:build_message(FixedJObj);
        {'error', Errors} -> {'error', Errors};
        'false' -> {'error', "API failed validation for route_resp"}
    end.

-spec resp_v(api_terms()) -> api_validator_resp().
-spec resp_v(api_terms(), wh_proplist()) -> api_validator_resp().
resp_v(API) -> resp_v(API, []).

resp_v(Props, Options) when is_list(Props) ->
    resp_v(wh_json:from_list(Props), Options);
resp_v(JObj, Options) ->
    {'ok', Schema} = kz_api:find_schema(<<"route_resp">>),
    jesse:validate_with_schema(Schema, JObj, [{'allowed_errors', 2} | Options]).

-spec win(api_terms()) -> api_builder_resp().
-spec win(api_terms(), wh_proplist()) -> api_builder_resp().
win(API) -> win(API, []).

win(Props, Options) when is_list(Props) ->
    win(wh_json:from_list(Props), Options);
win(JObj, Options) ->
    case win_v(JObj, Options) of
        {'ok', FixedJObj} ->
            kz_api:build_message(FixedJObj);
        {'error', Errors} -> {'error', Errors};
        'false' -> {'error', "API failed validation for route_win"}
    end.

-spec win_v(api_terms()) -> api_validator_resp().
-spec win_v(api_terms(), wh_proplist()) -> api_validator_resp().
win_v(API) -> win_v(API, []).

win_v(Props, Options) when is_list(Props) ->
    win_v(wh_json:from_list(Props), Options);
win_v(JObj, Options) ->
    {'ok', Schema} = kz_api:find_schema(<<"route_win">>),
    jesse:validate_with_schema(Schema, JObj, [{'allowed_errors', 2} | Options]).

%%--------------------------------------------------------------------
%% @doc Publish the JSON iolist() to the proper Exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_req(api_terms()) -> 'ok'.
-spec publish_req(api_terms(), binary()) -> 'ok'.
publish_req(JObj) ->
    publish_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Req, ContentType) ->
    {'ok', Payload} = ?MODULE:req(Req),
    amqp_util:callmgr_publish(Payload, ContentType, get_route_req_routing(Req)).

-spec publish_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_resp(RespQ, JObj) ->
    publish_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_resp(RespQ, Resp, ContentType) ->
    {'ok', Payload} = ?MODULE:resp(Resp),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_win(ne_binary(), api_terms()) -> 'ok'.
-spec publish_win(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_win(WinQ, JObj) ->
    publish_win(WinQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_win(WinQ, Win, ContentType) ->
    {'ok', Payload} = ?MODULE:win(Win),
    amqp_util:targeted_publish(WinQ, Payload, ContentType).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Helpers
%% @end
%%-----------------------------------------------------------------------------
-spec get_route_req_routing(api_terms()) -> ne_binary().
-spec get_route_req_routing(ne_binary(), ne_binary()) -> ne_binary().
get_route_req_routing(Api) ->
    {U, R} = get_auth_user_realm(Api),
    get_route_req_routing(R, U).

get_route_req_routing(Realm, User) when is_binary(Realm), is_binary(User) ->
    list_to_binary([?KEY_ROUTE_REQ
                    ,".", amqp_util:encode(Realm)
                    ,".", amqp_util:encode(User)
                   ]).

-spec get_auth_realm(api_terms()) -> ne_binary().
get_auth_realm(ApiProps) when is_list(ApiProps) ->
    [_ReqUser, ReqDomain]
        = binary:split(props:get_value(<<"From">>, ApiProps), <<"@">>),
    ReqDomain;
get_auth_realm(ApiJObj) ->
    [_ReqUser, ReqDomain]
        = binary:split(wh_json:get_value(<<"From">>, ApiJObj), <<"@">>),
    ReqDomain.

-spec get_auth_user(api_terms()) -> ne_binary().
get_auth_user(ApiProps) when is_list(ApiProps) ->
    [ReqUser, _ReqDomain]
        = binary:split(props:get_value(<<"From">>, ApiProps), <<"@">>),
    ReqUser;
get_auth_user(ApiJObj) ->
    [ReqUser, _ReqDomain]
        = binary:split(wh_json:get_value(<<"From">>, ApiJObj), <<"@">>),
    ReqUser.

-spec get_auth_user_realm(api_terms()) -> {ne_binary(), ne_binary()}.
get_auth_user_realm(ApiProps) when is_list(ApiProps) ->
    [ReqUser, ReqDomain]
        = binary:split(props:get_value(<<"From">>, ApiProps), <<"@">>),
    {ReqUser, ReqDomain};
get_auth_user_realm(ApiJObj) ->
    [ReqUser, ReqDomain]
        = binary:split(wh_json:get_value(<<"From">>, ApiJObj), <<"@">>),
    {ReqUser, ReqDomain}.

-spec is_actionable_resp(api_terms()) -> boolean().
is_actionable_resp(Props) when is_list(Props) ->
    case props:get_value(<<"Method">>, Props) of
        <<"bridge">> -> 'true';
        <<"park">> -> 'true';
        <<"error">> ->
            props:is_true(<<"Defer-Response">>, Props);
        _ -> 'false'
    end;
is_actionable_resp(JObj) ->
    is_actionable_resp(wh_json:to_proplist(JObj)).
