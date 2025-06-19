-module(swag_server_apikeys_router).

-export([get_paths/1]).
-export([get_paths/2]).
-export([get_operation/1]).
-export([get_operations/0]).

-type operations() :: #{
    Method :: binary() => OperationID :: swag_server_apikeys:operation_id()
}.

-type logic_handler(T) :: swag_server_apikeys:logic_handler(T).

-type swagger_handler_opts() :: #{
    validation_opts => swag_server_apikeys_validation:validation_opts()
}.

-type init_opts() :: {
    Operations      :: operations(),
    LogicHandler    :: logic_handler(_),
    SwaggerHandlerOpts :: swagger_handler_opts()
}.

-type operation_spec() :: #{
    path    := '_' | iodata(),
    method  := binary(),
    handler := module()
}.

-export_type([swagger_handler_opts/0]).
-export_type([init_opts/0]).
-export_type([operation_spec/0]).

-spec get_paths(LogicHandler :: logic_handler(_)) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler) ->
    get_paths(LogicHandler, #{}).

-spec get_paths(LogicHandler :: logic_handler(_), SwaggerHandlerOpts :: swagger_handler_opts()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler, SwaggerHandlerOpts) ->
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, SwaggerHandlerOpts}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

-spec get_operation(swag_server_apikeys:operation_id()) ->
   operation_spec().

get_operation(OperationId) ->
    maps:get(OperationId, get_operations()).

-spec get_operations() -> #{
    swag_server_apikeys:operation_id() := operation_spec()
}.

get_operations() ->
    #{ 
        'GetApiKey' => #{
            path => "/apikeys/v2/orgs/:partyId/api-keys/:apiKeyId",
            method => <<"GET">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        },
        'GetApiKeyPrivate' => #{
            path => "/apikeys/v2/priv/:partyId/api-keys/:apiKeyId",
            method => <<"GET">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        },
        'IssueApiKey' => #{
            path => "/apikeys/v2/orgs/:partyId/api-keys",
            method => <<"POST">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        },
        'IssueApiKeyPrivate' => #{
            path => "/apikeys/v2/priv/:partyId/api-keys",
            method => <<"POST">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        },
        'ListApiKeys' => #{
            path => "/apikeys/v2/orgs/:partyId/api-keys",
            method => <<"GET">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        },
        'ListApiKeysPrivate' => #{
            path => "/apikeys/v2/priv/:partyId/api-keys",
            method => <<"GET">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        },
        'RequestRevokeApiKey' => #{
            path => "/apikeys/v2/orgs/:partyId/api-keys/:apiKeyId/status",
            method => <<"PUT">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        },
        'RequestRevokeApiKeyPrivate' => #{
            path => "/apikeys/v2/priv/:partyId/api-keys/:apiKeyId/status",
            method => <<"PUT">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        },
        'RevokeApiKey' => #{
            path => "/apikeys/v2/orgs/:partyId/revoke-api-key/:apiKeyId",
            method => <<"GET">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        },
        'RevokeApiKeyPrivate' => #{
            path => "/apikeys/v2/priv/:partyId/revoke-api-key/:apiKeyId",
            method => <<"GET">>,
            handler => 'swag_server_apikeys_api_keys_handler'
        }
    }.
