
 This is a helper module.
 you can write some Javascript, parse it to the javascript AST
 on http://esprima.org/demo
 and copy it into here to generate the body of the unit tests
 in to_jast.erl

```erlang
    -module(test_utils).

    -export([
             generate_switch/0,
             generate_args/0,
             generate_fn/0,
             generate_return/0,
             generate_declarations/0
            ]).

    generate_switch() ->
        J = "{
                \"type\": \"SwitchStatement\",
                \"discriminant\": {
                    \"type\": \"Identifier\",
                    \"name\": \"args\"
                },
                \"cases\": [
                    {
                        \"type\": \"SwitchCase\",
                        \"test\": {
                            \"type\": \"Literal\",
                            \"value\": 0,
                            \"raw\": \"0\"
                        },
                        \"consequent\": [
                            {
                                \"type\": \"ExpressionStatement\",
                                \"expression\": {
                                    \"type\": \"Literal\",
                                    \"value\": \"erk\",
                                    \"raw\": \"\\\"erk\\\"\"
                                }
                            },
                            {
                                \"type\": \"BreakStatement\",
                                \"label\": null
                            }
                        ]
                    },
                    {
                        \"type\": \"SwitchCase\",
                        \"test\": {
                            \"type\": \"Literal\",
                            \"value\": 1,
                            \"raw\": \"1\"
                        },
                        \"consequent\": [
                            {
                                \"type\": \"ExpressionStatement\",
                                \"expression\": {
                                    \"type\": \"Literal\",
                                    \"value\": \"jerk\",
                                    \"raw\": \"\\\"jerk\\\"\"
                                }
                            },
                            {
                                \"type\": \"BreakStatement\",
                                \"label\": null
                            }
                        ]
                    },
                    {
                        \"type\": \"SwitchCase\",
                        \"test\": null,
                        \"consequent\": [
                            {
                                \"type\": \"ExpressionStatement\",
                                \"expression\": {
                                    \"type\": \"Literal\",
                                    \"value\": \"shirk\",
                                    \"raw\": \"\\\"shirk\\\"\"
                                }
                            }
                        ]
                    }
                ]
            }",
        {ok, Json, []} = rfc4627:decode(J),
        JStr = io_lib:format("~p", [Json]),
        make_utils:plain_log(JStr, "/tmp/test_utils.txt").

    generate_args() ->
        J = "{
          \"type\": \"ExpressionStatement\",
          \"expression\": {
            \"type\": \"AssignmentExpression\",
            \"operator\": \"=\",
            \"left\": {
              \"type\": \"Identifier\",
              \"name\": \"_args\"
             },
            \"right\": {
              \"type\": \"CallExpression\",
              \"callee\": {
                \"type\": \"MemberExpression\",
                \"computed\": false,
                \"object\": {
                  \"type\": \"Identifier\",
                  \"name\": \"arguments\"
                 },
                \"property\": {
                  \"type\": \"Identifier\",
                  \"name\": \"length\"
                 }
               },
              \"arguments\": []
             }
           }
         }",
        {ok, Json, []} = rfc4627:decode(J),
        JStr = io_lib:format("~p", [Json]),
        make_utils:plain_log(JStr, "/tmp/test_utils.txt").

    generate_fn() ->
        J = "{
                \"type\": \"ExpressionStatement\",
                \"expression\": {
                    \"type\": \"AssignmentExpression\",
                    \"operator\": \"=\",
                    \"left\": {
                        \"type\": \"Identifier\",
                        \"name\": \"simplefn\"
                    },
                    \"right\": {
                        \"type\": \"FunctionExpression\",
                        \"id\": null,
                        \"params\": [],
                        \"defaults\": [],
                        \"body\": {
                            \"type\": \"BlockStatement\",
                            \"body\": [
                                {
                                    \"type\": \"ReturnStatement\",
                                    \"argument\": {
                                        \"type\": \"Literal\",
                                        \"value\": \"banjolette\",
                                        \"raw\": \"\\\"banjolette\\\"\"
                                    }
                                }
                            ]
                        },
                        \"rest\": null,
                        \"generator\": false,
                        \"expression\": false
                    }
                }
            }",
        {ok, Json, []} = rfc4627:decode(J),
        JStr = io_lib:format("~p", [Json]),
        make_utils:plain_log(JStr, "/tmp/test_utils.txt").

```
 var fn = function () {
 	var a;
 	var b;
 	a = 1;
 	b = 2;
 	return a/b;
 	}
```erlang
    generate_return() ->
        J = "[
            {
                \"type\": \"VariableDeclaration\",
                \"declarations\": [
                    {
                        \"type\": \"VariableDeclarator\",
                        \"id\": {
                            \"type\": \"Identifier\",
                            \"name\": \"fn\"
                        },
                        \"init\": {
                            \"type\": \"FunctionExpression\",
                            \"id\": null,
                            \"params\": [],
                            \"defaults\": [],
                            \"body\": {
                                \"type\": \"BlockStatement\",
                                \"body\": [
                                    {
                                        \"type\": \"VariableDeclaration\",
                                        \"declarations\": [
                                            {
                                                \"type\": \"VariableDeclarator\",
                                                \"id\": {
                                                    \"type\": \"Identifier\",
                                                    \"name\": \"a\"
                                                },
                                                \"init\": null
                                            }
                                        ],
                                        \"kind\": \"var\"
                                    },
                                    {
                                        \"type\": \"VariableDeclaration\",
                                        \"declarations\": [
                                            {
                                                \"type\": \"VariableDeclarator\",
                                                \"id\": {
                                                    \"type\": \"Identifier\",
                                                    \"name\": \"b\"
                                                },
                                                \"init\": null
                                            }
                                        ],
                                        \"kind\": \"var\"
                                    },
                                    {
                                        \"type\": \"ExpressionStatement\",
                                        \"expression\": {
                                            \"type\": \"AssignmentExpression\",
                                            \"operator\": \"=\",
                                            \"left\": {
                                                \"type\": \"Identifier\",
                                                \"name\": \"a\"
                                            },
                                            \"right\": {
                                                \"type\": \"Literal\",
                                                \"value\": 1,
                                                \"raw\": \"1\"
                                            }
                                        }
                                    },
                                    {
                                        \"type\": \"ExpressionStatement\",
                                        \"expression\": {
                                            \"type\": \"AssignmentExpression\",
                                            \"operator\": \"=\",
                                            \"left\": {
                                                \"type\": \"Identifier\",
                                                \"name\": \"b\"
                                            },
                                            \"right\": {
                                                \"type\": \"Literal\",
                                                \"value\": 2,
                                                \"raw\": \"2\"
                                            }
                                        }
                                    },
                                    {
                                        \"type\": \"ReturnStatement\",
                                        \"argument\": {
                                            \"type\": \"BinaryExpression\",
                                            \"operator\": \"/\",
                                            \"left\": {
                                                \"type\": \"Identifier\",
                                                \"name\": \"a\"
                                            },
                                            \"right\": {
                                                \"type\": \"Identifier\",
                                                \"name\": \"b\"
                                            }
                                        }
                                    }
                                ]
                            },
                            \"rest\": null,
                            \"generator\": false,
                            \"expression\": false
                        }
                    }
                ],
                \"kind\": \"var\"
            }
        ]",
        {ok, Json, []} = rfc4627:decode(J),
        JStr = io_lib:format("~p", [Json]),
        make_utils:plain_log(JStr, "/tmp/test_utils.txt").

    generate_declarations() ->
        J = "[{
                \"type\": \"VariableDeclaration\",
                \"declarations\": [
                    {
                        \"type\": \"VariableDeclarator\",
                        \"id\": {
                            \"type\": \"Identifier\",
                            \"name\": \"a\"
                        },
                        \"init\": null
                    }
                ],
                \"kind\": \"var\"
            },
            {
                \"type\": \"VariableDeclaration\",
                \"declarations\": [
                    {
                        \"type\": \"VariableDeclarator\",
                        \"id\": {
                            \"type\": \"Identifier\",
                            \"name\": \"b\"
                        },
                        \"init\": null
                    }
                ],
                \"kind\": \"var\"
            }]",
        {ok, Json, []} = rfc4627:decode(J),
        JStr = io_lib:format("~p", [Json]),
        make_utils:plain_log(JStr, "/tmp/test_utils.txt").

```
