<img src='https://raw.github.com/hypernumbers/LuvvieScript/master/priv/images/luvviescript.png' />

What Is LuvvieScript?
---------------------

LuvvieScript is Erlang that compiles to Javascript - it is a lightweight, actor-based DOM scripting language for use in web browsers.

What Are Actors?
----------------

The Actor model is a progamming technique where units of concurrency share no state and communicate by sending each other messages. These units of concurrency are properly called **processes** (and not threads).

Why LuvvieScript And Not CoffeeScript?
--------------------------------------

LuvvieScript is designed to provide an impedance free development environment for web applications: where the back end is written in Erlang/OTP. Back-end Erlang processes can transparently send messages to brower-based Erlang processes - and vicky-verka...

Watch <a href='http://prezi.com/igzrai0bbpc5/?utm_campaign=share&utm_medium=copy'>the presentation</a>.

Why Is It Called LuvvieScript?
------------------------------

Most speakers of English have American English as their first dialect. In Blighty the word **luvvie** is slang for actor - allegedly from their habit of not knowing anyone's name and referring to everyone as darling or luvvie. And if I come across another project called **somethingerl** I will scream, I swear it...

Contributing
============

Contributing to LuvvieScript is easy - understand how the project is developing and then get cracking. The next few sections outline what you need to know, and then at the end there is a getting started section.

Overview
--------

The LuvvieScript compiler is being built up from the bottom. The logical structure of the LuvvieScript environment is:

    a RUNTIME running
          BEHAVIOURS (eg gen_dom) which are implemented in
              MODULES which consist of
                  ATTRIBUTES and
                  FUNCTIONS which are composed of
                      CLAUSES which are made up of
                           OPERATORS acting on
                                PRIMITIVES

LuvvieScript Primitives
-----------------------

    Erlang &             Javascript
    LuvvieSript

    Integer        <---> Integer
    Float          <---> Double
    String         <---> UTF8 string
    Atom           <---> object {atom: Atom}
    List           <---> Array
    Records        <---> Objects
                         with a helper fn to map position to element name
    Funs           <---> Funs
    Tuple          <---> {tuple: Array}
    Dict Of Dicts  <---> Objects/JSON
                         All Fn calls out to external fns will be cast on return

    Binaries       <--->  none

Development Priorities
----------------------

There are three productions required on compiling Erlang/LuvvieScript to Javascript:

* javascript
* Erlang/LuvvieScript pretty printed for the browser debugger
* sourcemaps that map the produced javascript to the underlying LuvvieScript.

The canonical site for information about sourcemaps is https://github.com/mozilla/source-map

The reason for this is the process by which compiled-to-Javascript languages can be debugged in browsers. The debugger steps though the **Javascript** and then uses the sourcemapper information to translate its location back to a position in the **original files**.

The Production ToolChain
------------------------

The production toolchain uses normal Erlang syntax tools to process and validate the Erlang onto a standard Erlang Abstract Syntax tree - which is then compiled to Javascript.

This process has a substantial problem that need to be addressed:

* normal Erlang syntax tools produce intermediate forms that are decorated with the **line number** they occur on in the original code whereas the source mapping files need **column information** as well.

The production process outlined here, although it seems convoluted, is designed to address this problems and generate the appropiate 3 productions.

The process of turninging Erlang/LuvvieScript code into Javascript follows this process:

* the raw Erlang is normalised by Erlang preprocessor to the ``.P`` form

* the normalised Erlang is compiled to the standard Erlang Abstract Syntax tree (AST) format
* the normalised AST is compiled to a set of pseudo-tokens which contain the linefile information of the ``.P`` module they were from

* The normalised Erlang (``.P``) is tokenized by the Erlang tokenizer to produce a token output that combines the Erlang symbols with the lines they occurred in the original script.

The two token scripts are processed in sync to generate the javascript and the associated sourcemap.

The ``.P`` form of the Erlang has the inserted code *reversed out* to generate the source code against which the sourcemap reports.

The Production Toolchain - Another View
---------------------------------------

This process is best shown with an example. Here is some raw Erlang sourcecode deliberately produced with erratic indenting.

```erlang

    -module(including).

    -compile(export_all).

    -include("included.hrl").

    including() ->


                #brannigan{}.

    location() ->      dingo().

    complex_clauses() ->
        A=1,
        B = case A of
                1 ->  "stick the return up here";
            3 ->
                    "stick it down here"
            end, C = 7,
        Z1 = [X + 2
         ||
            X <- [A, B, C]],
        Z2 = [X + 2 || X <- [A, B, C]],
        {Z1, Z2}.

```

This is preprocessed to the ``.P`` format:

```erlang

    -file("/home/vagrant/LuvvieScript/test/supported/including.erl", 1).

    -module(including).

    -compile(export_all).

    -file("/home/vagrant/LuvvieScript/test/supported/../include/included.hrl",
          1).

    -record(brannigan,{hip,hop,'hippety-hop','dont stop'}).

    dingo() ->
        bogbrush.

    -file("/home/vagrant/LuvvieScript/test/supported/including.erl", 6).

    including() ->
        #brannigan{}.

    location() ->
        dingo().

    complex_clauses() ->
        A = 1,
        B = case A of
                1 ->
                    "stick the return up here";
                3 ->
                    "stick it down here"
            end,
        C = 7,
        Z1 =
            [
             X + 2 ||
                 X <- [A,B,C]
            ],
        Z2 =
            [
             X + 2 ||
                 X <- [A,B,C]
            ],
        {Z1,Z2}.

```

Three things to notice:

* the include files have been included
* there are ``file`` attributes inserted which tell you where the inserted information came from
* the complex clauses of the list comprehensions have been pretty-printed

The Erlang AST to which this has been compiled is a Lisp dialect, as shown in this extract:

```erlang
    [{attribute,1,file,
         {"/home/vagrant/LuvvieScript/test/supported/including.P",1}},
     {attribute,-1,file,
         {"/home/vagrant/LuvvieScript/test/supported/including.erl",1}},
     {attribute,3,module,including},
     {attribute,5,compile,export_all},
     {attribute,-7,file,
         {"/home/vagrant/LuvvieScript/test/supported/../include/included.hrl",1}},
     {attribute,4,record,
         {brannigan,
             [{record_field,4,{atom,4,hip}},
              {record_field,4,{atom,4,hop}},
              {record_field,4,{atom,4,'hippety-hop'}},
              {record_field,4,{atom,4,'dont stop'}}]}},
     {function,6,dingo,0,[{clause,6,[],[],[{atom,7,bogbrush}]}]},
     {attribute,-9,file,
         {"/home/vagrant/LuvvieScript/test/supported/including.erl",6}},
     {function,8,including,0,[{clause,8,[],[],[{record,9,brannigan,[]}]}]},
     {function,11,location,0,[{clause,11,[],[],[{call,12,{atom,12,dingo},[]}]}]},
     {function,14,complex_clauses,0,
         [{clause,14,[],[],
```

Once this has been transformed so the line numbers have been replaced by a tuple of file name and line no, it can be **translated** into javascript - the compilation step is fairly trivial.

At the moment the javascript being produced is only fragmentary, but these fragments can be used to demonstrate the intermediate pseudo-tokens.

Take the Erlang Clause:

```erlang
    types() ->
        A  = 1,
        B  = 2.3,
        C1 = true,
        C2 = blue,
        C3 = 'Blue',
        D  = [a, b],
        E  = {1, 2},
        F  = "string",
        G = make_ref(),
        {A, B, C1, C2, C3, D, E, F, G}.

```

This is turned into javascript pseudo-tokens as shown below:

```erlang
    [{var,"A",{"types.erl",8}},
     {match,"=",{"types.erl",8}},
     {int,"1",{"types.erl",8}},
     {linending,";~n",nonce},
     {var,"B",{"types.erl",9}},
     {match,"=",{"types.erl",9}},
     {float,"2.29999999999999982236e+00", {"types.erl",9}},
     {linending,";~n",nonce},
     {var,"C1",{"types.erl",10}},
     {match,"=",{"types.erl",10}},
     {atom,"{atom: \"true\"}",{"types.erl",10}},
     {linending,";~n",nonce},
     {var,"C2",{"types.erl",11}},
     {match,"=",{"types.erl",11}},
     {atom,"{atom: \"blue\"}",{"types.erl",11}},
     {linending,";~n",nonce},
     ...
     {linending,";~n",nonce}]}]}]}
```

These pseudo-tokens can be turned into Javascript trivially (with an appropriate indentation). By matching these pseudo-tokens with the original token stream the sourcemap can be generated. The original Erlang tokens for this clause being:

```erlang
    {var,6,'A'},
    {white_space,6,"  "},
    {'=',6},
    {white_space,6," "},
    {integer,6,1},
    {',',6},
    {white_space,6,"\n    "},
    {var,7,'B'},
    {white_space,7,"  "},
    {'=',7},
    {white_space,7," "},
    {float,7,2.3},
    {',',7},
    ...
```

Towards A Test Suite
--------------------

The next stage of LuvvieScript is a running test suite. This section will outline the testing process.

In ``test/supported`` there are a series of Erlang files that contain example code of supported constructs. In ``test/unsupported`` there are a similar set of files of unsupported constructs - a lot of unsupported constructs are BIFs and functions in the ``erlang`` module that don't make any sense in this environment. For example the BIF ``erlang:bump_reduction/0`` only makes sense in the context of the Erlang VM and will never be implemented in LuvvieScript.

The test strategy is this:
* compile all the files in ``test/supported`` as Erlang
* execute all the ``arity/0`` functions they export
* compile all the files in ``test/supported`` as LuvvieScript
* execute all the ``arity/0`` functions they export
* assert the results are identical

The test suite will be built in two phases - basic test followed by more advanced ones **once we have some full working end-to-end results which include javascript and source maps**.

Contributing
============

The contribution cycle is this:

* read the issues list at https://github.com/hypernumbers/LuvvieScript/issues
* find a file in ``test/supported`` that is not making the appropriate productions and which nobody has claimed (or write one that addresses something none of the existing ones do)
* raise an issue of type ``enhancement`` against the test file name
* fix and raise a pull request

<div style="margin:20px;padding:20px;background-color:#000;color:#fff'>
<h1>If You Have Read This Far</h1>
<p>Follow me on Twitter @gordonguthrie</p>
<p>Fork this repo and get hacking</p>
</div>