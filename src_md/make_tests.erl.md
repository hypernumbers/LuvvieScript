```erlang
    -module(make_tests).

    -export([
             generate/0
            ]).

    generate() ->
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
        make_utils:plain_log(JStr, "/tmp/make_tests.txt").
```
