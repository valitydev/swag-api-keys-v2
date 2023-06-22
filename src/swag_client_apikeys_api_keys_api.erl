%% -*- mode: erlang -*-
-module(swag_client_apikeys_api_keys_api).

%% generated methods

-export([get_api_key/2]).
-export([get_api_key/3]).

-export([issue_api_key/2]).
-export([issue_api_key/3]).

-export([list_api_keys/2]).
-export([list_api_keys/3]).

-export([request_revoke_api_key/2]).
-export([request_revoke_api_key/3]).

-export([revoke_api_key/2]).
-export([revoke_api_key/3]).


-spec get_api_key(Endpoint :: swag_client_apikeys:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_api_key(Endpoint, Params) ->
    get_api_key(Endpoint, Params, []).

-spec get_api_key(Endpoint :: swag_client_apikeys:endpoint(), Params :: map(), Opts :: swag_client_apikeys:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_api_key(Endpoint, Params, Opts) ->
    process_response(swag_client_apikeys_procession:process_request(
        get,
        swag_client_apikeys_utils:get_url(Endpoint, "/apikeys/v2/orgs/:partyId/api-keys/:apiKeyId"),
        Params,
        get_request_spec(get_api_key),
        Opts
    ), get_api_key).

-spec issue_api_key(Endpoint :: swag_client_apikeys:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_api_key(Endpoint, Params) ->
    issue_api_key(Endpoint, Params, []).

-spec issue_api_key(Endpoint :: swag_client_apikeys:endpoint(), Params :: map(), Opts :: swag_client_apikeys:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_api_key(Endpoint, Params, Opts) ->
    process_response(swag_client_apikeys_procession:process_request(
        post,
        swag_client_apikeys_utils:get_url(Endpoint, "/apikeys/v2/orgs/:partyId/api-keys"),
        Params,
        get_request_spec(issue_api_key),
        Opts
    ), issue_api_key).

-spec list_api_keys(Endpoint :: swag_client_apikeys:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_api_keys(Endpoint, Params) ->
    list_api_keys(Endpoint, Params, []).

-spec list_api_keys(Endpoint :: swag_client_apikeys:endpoint(), Params :: map(), Opts :: swag_client_apikeys:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_api_keys(Endpoint, Params, Opts) ->
    process_response(swag_client_apikeys_procession:process_request(
        get,
        swag_client_apikeys_utils:get_url(Endpoint, "/apikeys/v2/orgs/:partyId/api-keys"),
        Params,
        get_request_spec(list_api_keys),
        Opts
    ), list_api_keys).

-spec request_revoke_api_key(Endpoint :: swag_client_apikeys:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
request_revoke_api_key(Endpoint, Params) ->
    request_revoke_api_key(Endpoint, Params, []).

-spec request_revoke_api_key(Endpoint :: swag_client_apikeys:endpoint(), Params :: map(), Opts :: swag_client_apikeys:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
request_revoke_api_key(Endpoint, Params, Opts) ->
    process_response(swag_client_apikeys_procession:process_request(
        put,
        swag_client_apikeys_utils:get_url(Endpoint, "/apikeys/v2/orgs/:partyId/api-keys/:apiKeyId/status"),
        Params,
        get_request_spec(request_revoke_api_key),
        Opts
    ), request_revoke_api_key).

-spec revoke_api_key(Endpoint :: swag_client_apikeys:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
revoke_api_key(Endpoint, Params) ->
    revoke_api_key(Endpoint, Params, []).

-spec revoke_api_key(Endpoint :: swag_client_apikeys:endpoint(), Params :: map(), Opts :: swag_client_apikeys:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
revoke_api_key(Endpoint, Params, Opts) ->
    process_response(swag_client_apikeys_procession:process_request(
        get,
        swag_client_apikeys_utils:get_url(Endpoint, "/apikeys/v2/orgs/:partyId/revoke-api-key/:apiKeyId"),
        Params,
        get_request_spec(revoke_api_key),
        Opts
    ), revoke_api_key).

process_response({ok, Code, Headers, RespBody}, OperationID) ->
    try swag_client_apikeys_procession:process_response(
        get_response_spec(OperationID, Code),
        RespBody
    ) of
        {ok, Resp} ->
            {ok, Code, Headers, Resp};
        Error ->
            Error
    catch
        error:invalid_response_code ->
            {error, {invalid_response_code, Code}}
    end;
process_response(Error, _) ->
    Error.


-spec get_request_spec(OperationID :: swag_client_apikeys:operation_id()) ->
    Spec :: swag_client_apikeys_procession:request_spec() | no_return().


get_request_spec('get_api_key') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'apiKeyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('issue_api_key') ->
    [
        {'partyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'ApiKeyIssue', #{
            source => body,
            rules  => [schema, {required, true}]
        }}
    ];
get_request_spec('list_api_keys') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'status', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['active', 'revoked']}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('request_revoke_api_key') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'apiKeyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'binary', #{
            source => body,
            rules  => [schema, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('revoke_api_key') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'apiKeyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'apiKeyRevokeToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 4000}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client_apikeys:operation_id(), Code :: swag_client_apikeys_procession:code()) ->
    Spec :: swag_client_apikeys_procession:response_spec() | no_return().


get_response_spec('get_api_key', 200) ->
    {'ApiKey', 'ApiKey'};

get_response_spec('get_api_key', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('get_api_key', 401) ->
    undefined;

get_response_spec('get_api_key', 404) ->
    undefined;

get_response_spec('issue_api_key', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};

get_response_spec('issue_api_key', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('issue_api_key', 401) ->
    undefined;

get_response_spec('list_api_keys', 200) ->
    {'inline_response_200', 'inline_response_200'};

get_response_spec('list_api_keys', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('list_api_keys', 401) ->
    undefined;

get_response_spec('list_api_keys', 404) ->
    undefined;

get_response_spec('request_revoke_api_key', 204) ->
    undefined;

get_response_spec('request_revoke_api_key', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('request_revoke_api_key', 401) ->
    undefined;

get_response_spec('request_revoke_api_key', 404) ->
    undefined;

get_response_spec('revoke_api_key', 204) ->
    undefined;

get_response_spec('revoke_api_key', 400) ->
    {'BadRequest', 'BadRequest'};

get_response_spec('revoke_api_key', 401) ->
    undefined;

get_response_spec('revoke_api_key', 404) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
