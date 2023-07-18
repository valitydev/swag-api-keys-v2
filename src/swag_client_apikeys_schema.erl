%% -*- mode: erlang -*-
-module(swag_client_apikeys_schema).

-export([get/0]).
-export([get_raw/0]).
-export([enumerate_discriminator_children/1]).

-define(DEFINITIONS, <<"definitions">>).

-spec get() -> swag_client_apikeys:object().
get() ->
    ct_expand:term(enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw()))).

-spec enumerate_discriminator_children(Schema :: map()) ->
    Schema :: map() | no_return().
enumerate_discriminator_children(Schema = #{?DEFINITIONS := Defs}) ->
    try
        Parents = enumerate_parents(Defs),
        DefsFixed = maps:fold(fun correct_definition/3, Defs, Parents),
        Schema#{?DEFINITIONS := DefsFixed}
    catch
        _:Error ->
            handle_error(Error)
    end;
enumerate_discriminator_children(_) ->
    handle_error(no_definitions).

-spec handle_error(_) ->
    no_return().
handle_error(Error) ->
    erlang:error({schema_invalid, Error}).

enumerate_parents(Definitions) ->
    maps:fold(
        fun
            (Name, #{<<"allOf">> := AllOf}, AccIn) ->
                lists:foldl(
                    fun
                        (#{<<"$ref">> := <<"#/definitions/", Parent/binary>>}, Acc) ->
                            Schema = maps:get(Parent, Definitions),
                            Discriminator = maps:get(<<"discriminator">>, Schema, undefined),
                            add_parent_child(Discriminator, Parent, Name, Acc);
                        (_Schema, Acc) ->
                            Acc
                    end,
                    AccIn,
                    AllOf
                );
            (Name, #{<<"discriminator">> := _}, Acc) ->
                add_parent(Name, Acc);
            (_Name, _Schema, AccIn) ->
                AccIn
        end,
        #{},
        Definitions
    ).

add_parent_child(undefined, _Parent, _Child, Acc) ->
    Acc;
add_parent_child(_Discriminator, Parent, Child, Acc) ->
    maps:put(Parent, [Child | maps:get(Parent, Acc, [])], Acc).

add_parent(Parent, Acc) when not is_map_key(Parent, Acc) ->
    maps:put(Parent, [], Acc);
add_parent(_Parent, Acc) ->
    Acc.

correct_definition(Parent, Children, Definitions) ->
    ParentSchema1 = maps:get(Parent, Definitions),
    Discriminator = maps:get(<<"discriminator">>, ParentSchema1),
    ParentSchema2 = deep_put([<<"properties">>, Discriminator, <<"enum">>], Children, ParentSchema1),
    maps:put(Parent, ParentSchema2, Definitions).

deep_put([K], V, M) ->
    M#{K => V};
deep_put([K | Ks], V, M) ->
    maps:put(K, deep_put(Ks, V, maps:get(K, M)), M).

-spec get_raw() -> map().
get_raw() ->
    #{
  <<"swagger">> => <<"2.0">>,
  <<"info">> => #{
    <<"description">> => <<"Vality API Keys Management API является интерфейсом для управления набором API-ключей, используемых для авторизации запросов к основному API с ваших бэкенд-сервисов. Любые сторонние приложения, включая ваш личный кабинет, являются внешними приложениями-клиентами данного API.\nМы предоставляем REST API поверх HTTP-протокола, схема которого описывается в соответствии со стандартом [OpenAPI 3][OAS3]. Коды возврата описываются соответствующими HTTP-статусами. Платформа принимает и возвращает значения JSON в теле запросов и ответов.\n[OAS3]: https://swagger.io/specification/\n## Идентификатор запроса\nПри любом обращении к API в заголовке `X-Request-ID` соответствующего запроса необходимо передать его уникальный идентификатор:\n```\n    X-Request-ID: 37d735d4-0f42-4f05-89fa-eaa478fb5aa9\n```\n## Формат содержимого\nЛюбой запрос к API должен выполняться в кодировке UTF-8 и с указанием содержимого в формате JSON.\n``` Content-Type: application/json; charset=utf-8 ```\n## Максимальное время обработки запроса\nПри любом обращении к API в заголовке `X-Request-Deadline` соответствующего запроса можно передать параметр отсечки по времени, определяющий максимальное время ожидания завершения операции по запросу:\n```\n    X-Request-Deadline: 10s\n```\nПо истечении указанного времени система прекращает обработку запроса. Рекомендуется указывать значение не более одной минуты, но не менее трёх секунд.\n`X-Request-Deadline` может:\n* задаваться в формате `date-time` согласно\n    [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339);\n* задаваться в относительных величинах: в миллисекундах (`150000ms`), секундах (`540s`) или\n    минутах (`3.5m`).\n">>,
    <<"version">> => <<"0.1.0">>,
    <<"title">> => <<"Vality Api Keys API">>,
    <<"termsOfService">> => <<"https://vality.dev/">>,
    <<"contact">> => #{
      <<"name">> => <<"Technical support team">>,
      <<"url">> => <<"https://api.vality.dev">>,
      <<"email">> => <<"support@vality.dev">>
    },
    <<"license">> => #{
      <<"name">> => <<"Apache 2.0">>,
      <<"url">> => <<"https://www.apache.org/licenses/LICENSE-2.0.html">>
    }
  },
  <<"host">> => <<"api.vality.dev">>,
  <<"basePath">> => <<"/apikeys/v2">>,
  <<"tags">> => [ #{
    <<"name">> => <<"apiKeys">>,
    <<"x-displayName">> => <<"API-ключи">>
  }, #{
    <<"name">> => <<"errorCodes">>,
    <<"description">> => <<"## Общие ошибки\nОшибки возникающие при попытках совершения недопустимых операций, операций с невалидными объектами или несуществующими ресурсами. Имеют следующий вид:\n```json {\n    \"code\": \"string\",\n    \"message\": \"string\"\n} ```\nВ поле `message` содержится информация по произошедшей ошибке. Например:\n```json {\n    \"code\": \"invalidRequest\",\n    \"message\": \"Property 'name' is required.\"\n} ```\n## Ошибки обработки запросов\nВ процессе обработки запросов силами нашей платформы могут происходить различные непредвиденные ситуации. Об их появлении платформа сигнализирует по протоколу HTTP соответствующими [статусами][5xx], обозначающими ошибки сервера.\n|  Код    |  Описание  | | ------- | ---------- | | **500** | В процессе обработки платформой запроса возникла непредвиденная ситуация. При получении подобного кода ответа мы рекомендуем обратиться в техническую поддержку. | | **503** | Платформа временно недоступна и не готова обслуживать данный запрос. Запрос гарантированно не выполнен, при получении подобного кода ответа попробуйте выполнить его позднее, когда доступность платформы будет восстановлена. | | **504** | Платформа превысила допустимое время обработки запроса, результат запроса не определён. Попробуйте отправить запрос повторно или выяснить результат выполнения исходного запроса, если повторное исполнение запроса нежелательно. |\n[5xx]: https://tools.ietf.org/html/rfc7231#section-6.6\n\nЕсли вы получили ошибку, которой нет в данном описании, обратитесь в техническую поддержку.\n">>,
    <<"x-displayName">> => <<"Коды ошибок">>
  } ],
  <<"schemes">> => [ <<"https">> ],
  <<"consumes">> => [ <<"application/json; charset=utf-8">> ],
  <<"produces">> => [ <<"application/json; charset=utf-8">> ],
  <<"security">> => [ #{
    <<"bearer">> => [ ]
  } ],
  <<"paths">> => #{
    <<"/orgs/{partyId}/api-keys">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Перечислить ключи организации">>,
        <<"operationId">> => <<"listApiKeys">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"status">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Фильтр по статусу ключа. По умолчанию `active`.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"active">>, <<"revoked">> ]
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Selection limit">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Ключи найдены">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Выпустить новый ключ">>,
        <<"operationId">> => <<"issueApiKey">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"apiKeyIssue">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/ApiKeyIssue">>
          }
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Ключ выпущен">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_1">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          }
        }
      }
    },
    <<"/orgs/{partyId}/api-keys/{apiKeyId}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Получить данные ключа">>,
        <<"operationId">> => <<"getApiKey">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"apiKeyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор ключа">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Ключ найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ApiKey">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/orgs/{partyId}/api-keys/{apiKeyId}/status">> => #{
      <<"put">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Запросить отзыв ключа">>,
        <<"description">> => <<"Просит отозвать Api Key, для подтверждения запроса\nпосылает на почту запросившего письмо с ссылкой на\nrevokeApiKey для подтверждения операции\n">>,
        <<"operationId">> => <<"requestRevokeApiKey">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"apiKeyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор ключа">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"requestRevoke">>,
          <<"description">> => <<"Status to change Api Key into">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/RequestRevoke">>
          }
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Запрос на операцию получен">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    },
    <<"/orgs/{partyId}/revoke-api-key/{apiKeyId}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Отозвать ключ">>,
        <<"description">> => <<"Ссылка на этот запрос приходит на почту запросившего\nrequestRevokeApiKey, в результате выполнения этого запроса\nApi Key будет отозван\n">>,
        <<"operationId">> => <<"revokeApiKey">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Unique identifier of the request to the system">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Maximum request processing time">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"apiKeyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор ключа">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"apiKeyRevokeToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен отзыва ключа">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 4000,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Ключ отозван">>
          },
          <<"400">> => #{
            <<"description">> => <<"Invalid input data for operation">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Authorization error">>
          },
          <<"404">> => #{
            <<"description">> => <<"The content you are looking for was not found">>
          }
        }
      }
    }
  },
  <<"securityDefinitions">> => #{
    <<"bearer">> => #{
      <<"description">> => <<"Use [JWT](https://jwt.io) for call authentication. The corresponding key is passed in the header.\n```shell\n Authorization: Bearer {YOUR_API_KEY_JWT}\n```\n">>,
      <<"type">> => <<"apiKey">>,
      <<"name">> => <<"Authorization">>,
      <<"in">> => <<"header">>
    }
  },
  <<"definitions">> => #{
    <<"AccessToken">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"accessToken">> ],
      <<"properties">> => #{
        <<"accessToken">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0a2kiOiIxS2dJWUJHc0NncSIsImlhdCI6MTUxNjIzOTAyMn0.6YsaZQC9A7BjxXHwRbJfUO6VujOb4rHTKrqmMt64TbQ\n">>,
          <<"description">> => <<"Токен доступа, ассоциированный с данным ключом">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        }
      },
      <<"example">> => #{
        <<"accessToken">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0a2kiOiIxS2dJWUJHc0NncSIsImlhdCI6MTUxNjIzOTAyMn0.6YsaZQC9A7BjxXHwRbJfUO6VujOb4rHTKrqmMt64TbQ\n">>
      }
    },
    <<"ApiKey">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"createdAt">>, <<"id">>, <<"name">>, <<"status">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"$ref">> => <<"#/definitions/ApiKeyID">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания">>,
          <<"readOnly">> => true
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"live-site-integration">>,
          <<"description">> => <<"Запоминающееся название ключа">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"status">> => #{
          <<"$ref">> => <<"#/definitions/ApiKeyStatus">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Произвольный набор данных, специфичный для клиента API и\nнепрозрачный для системы\n">>,
          <<"properties">> => #{ }
        }
      },
      <<"description">> => <<"Ключ для авторизации запросов к API">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"metadata">> => <<"{}">>,
        <<"name">> => <<"live-site-integration">>,
        <<"id">> => <<"1KgIYBGsCgq">>,
        <<"status">> => #{ }
      }
    },
    <<"ApiKeyID">> => #{
      <<"type">> => <<"string">>,
      <<"minLength">> => 1,
      <<"maxLength">> => 40,
      <<"description">> => <<"Идентификатор ключа">>,
      <<"example">> => <<"1KgIYBGsCgq">>
    },
    <<"ApiKeyIssue">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"name">> ],
      <<"properties">> => #{
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"live-site-integration">>,
          <<"description">> => <<"Запоминающееся название ключа">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Произвольный набор данных, специфичный для клиента API и\nнепрозрачный для системы\n">>,
          <<"properties">> => #{ }
        }
      },
      <<"description">> => <<"Параметры создания ключа для авторизации запросов к API">>,
      <<"example">> => #{
        <<"metadata">> => <<"{}">>,
        <<"name">> => <<"live-site-integration">>
      }
    },
    <<"ApiKeyStatus">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Статус ключа">>,
      <<"enum">> => [ <<"active">>, <<"revoked">> ]
    },
    <<"BadRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"errorType">> ],
      <<"properties">> => #{
        <<"errorType">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"NotFound">>,
          <<"description">> => <<"Error type">>,
          <<"enum">> => [ <<"SchemaViolated">>, <<"NotFound">>, <<"WrongType">>, <<"NotInRange">>, <<"WrongSize">>, <<"WrongLength">>, <<"WrongArray">>, <<"NoMatch">>, <<"InvalidToken">> ]
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"X-Request-ID">>,
          <<"description">> => <<"Name or identifier of message element containing invalid data">>
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Required parameter was not sent">>,
          <<"description">> => <<"Explanation of why the data is invalid">>
        }
      }
    },
    <<"ContinuationToken">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
    },
    <<"RequestRevoke">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Status to change Api Key into">>,
          <<"enum">> => [ <<"revoked">> ]
        }
      },
      <<"description">> => <<"Параметры отзыва ключа">>,
      <<"example">> => #{
        <<"status">> => <<"revoked">>
      }
    },
    <<"inline_response_200">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"results">> ],
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"A token signalling that only part of the data has been transmitted in the response.\nTo retrieve the next part, you need repeat the request to the service again, specifying the same set of conditions and the received token.\nIf there is no token, the last piece of data is received.\n">>
        },
        <<"results">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/ApiKey">>
          }
        }
      },
      <<"example">> => #{
        <<"results">> => [ #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => <<"{}">>,
          <<"name">> => <<"live-site-integration">>,
          <<"id">> => <<"1KgIYBGsCgq">>,
          <<"status">> => #{ }
        }, #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => <<"{}">>,
          <<"name">> => <<"live-site-integration">>,
          <<"id">> => <<"1KgIYBGsCgq">>,
          <<"status">> => #{ }
        } ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_1">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"AccessToken">> => #{
          <<"$ref">> => <<"#/definitions/AccessToken">>
        },
        <<"ApiKey">> => #{
          <<"$ref">> => <<"#/definitions/ApiKey">>
        }
      },
      <<"example">> => #{
        <<"ApiKey">> => #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => <<"{}">>,
          <<"name">> => <<"live-site-integration">>,
          <<"id">> => <<"1KgIYBGsCgq">>,
          <<"status">> => #{ }
        },
        <<"AccessToken">> => #{
          <<"accessToken">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0a2kiOiIxS2dJWUJHc0NncSIsImlhdCI6MTUxNjIzOTAyMn0.6YsaZQC9A7BjxXHwRbJfUO6VujOb4rHTKrqmMt64TbQ\n">>
        }
      }
    }
  },
  <<"parameters">> => #{
    <<"requestID">> => #{
      <<"name">> => <<"X-Request-ID">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Unique identifier of the request to the system">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 32,
      <<"minLength">> => 1
    },
    <<"deadline">> => #{
      <<"name">> => <<"X-Request-Deadline">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Maximum request processing time">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"apiKeyId">> => #{
      <<"name">> => <<"apiKeyId">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор ключа">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"apiKeyRevokeToken">> => #{
      <<"name">> => <<"apiKeyRevokeToken">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Токен отзыва ключа">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 4000,
      <<"minLength">> => 1
    },
    <<"partyId">> => #{
      <<"name">> => <<"partyId">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор участника">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"limit">> => #{
      <<"name">> => <<"limit">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Selection limit">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"maximum">> => 1000,
      <<"minimum">> => 1,
      <<"format">> => <<"int32">>
    }
  },
  <<"responses">> => #{
    <<"BadRequest">> => #{
      <<"description">> => <<"Invalid input data for operation">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/BadRequest">>
      }
    },
    <<"NotFound">> => #{
      <<"description">> => <<"The content you are looking for was not found">>
    },
    <<"Unauthorized">> => #{
      <<"description">> => <<"Authorization error">>
    }
  }
}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA,
  <<"{\"definitions\": {
       \"Pet\": {
         \"type\":          \"object\",
         \"discriminator\": \"petType\",
         \"properties\": {
            \"name\":    {\"type\": \"string\"},
            \"petType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"petType\"]
       },
       \"Cat\": {
         \"description\": \"A representation of a cat\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"huntingSkill\": {
                 \"type\":        \"string\",
                 \"description\": \"The measured skill for hunting\",
                 \"default\":     \"lazy\",
                 \"enum\":        [\"clueless\", \"lazy\", \"adventurous\", \"aggressive\"]
               }
             },
             \"required\": [\"huntingSkill\"]
           }
         ]
       },
       \"Dog\": {
         \"description\": \"A representation of a dog\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"packSize\": {
                 \"type\":        \"integer\",
                 \"format\":      \"int32\",
                 \"description\": \"the size of the pack the dog is from\",
                 \"default\":     0,
                 \"minimum\":     0
               }
             }
           }
         ],
         \"required\": [\"packSize\"]
       },
       \"Person\": {
         \"type\":          \"object\",
         \"discriminator\": \"personType\",
         \"properties\": {
           \"name\": {\"type\": \"string\"},
           \"sex\": {
             \"type\": \"string\",
             \"enum\": [\"male\", \"female\"]
           },
           \"personType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"sex\", \"personType\"]
       },
       \"WildMix\": {
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {\"$ref\": \"#/definitions/Person\"}
         ],
       },
       \"Dummy\": {
         \"type\":          \"object\",
         \"discriminator\": \"dummyType\",
         \"properties\": {
           \"name\":      {\"type\": \"string\"},
           \"dummyType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"dummyType\"]
       }
     }}">>).

get_enum(Parent, Discr, Schema) ->
    lists:sort(deep_get([?DEFINITIONS, Parent, <<"properties">>, Discr, <<"enum">>], Schema)).

deep_get([K], M) ->
    maps:get(K, M);
deep_get([K | Ks], M) ->
    deep_get(Ks, maps:get(K, M)).

-spec test() -> _.
-spec enumerate_discriminator_children_test() -> _.
enumerate_discriminator_children_test() ->
    Schema      = jsx:decode(?SCHEMA, [return_maps]),
    FixedSchema = enumerate_discriminator_children(Schema),
    ?assertEqual(lists:sort([<<"Dog">>, <<"Cat">>, <<"WildMix">>]), get_enum(<<"Pet">>, <<"petType">>, FixedSchema)),
    ?assertEqual([<<"WildMix">>], get_enum(<<"Person">>,  <<"personType">>, FixedSchema)),
    ?assertEqual([],              get_enum(<<"Dummy">>,   <<"dummyType">>,  FixedSchema)).

-spec get_test() -> _.
get_test() ->
    ?assertEqual(
       enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw())),
       ?MODULE:get()
    ).
-endif.
