Differences Between LuvvieScript And Erlang
===========================================

LuvvieScript integers are expressed as doubles - whereas Erlang ones are bignums.

There is no type difference between an int and a float in LuvvieScript so the following operations are different

In erlang:

```erlang
   1 ==  1.0 -> true
   1 =:= 1.0 -> false
   1 /=  1.0 -> false
   1=/= 1.0  -> true
```
In LuvvieScript:

```erlang
   1 ==  1.0 -> true
   1 =:= 1.0 -> true
   1 /=  1.0 -> false
   1 =/= 1.0 -> false
```
