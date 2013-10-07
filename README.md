<img src='https://raw.github.com/hypernumbers/LuvvieScript/master/priv/images/luvviescript.png' />

Note
----

This README is not fully up to date. Where sections are awaiting a rewrite they will be marked...

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

Modailities
===========

To make it easier for non-Erlang programmers to join in this project it is written in Literate Erlang and not plain Erlang.

Literate Erlang is designed to work better in GitHub - the source code is properly readable - it is basically Erlang embedded inside Markdown.

Compiling Literate Erlang is a two part process:
``rebar compile_literate`` converts the Literate Erlang in ``src_md/`` and ``include_md`` into its plain Erlang equivalents in ``src/`` and ``include/``. The plain Erlang can then be compiled using ``rebar compile``.

Please see the README of Literate Erlang at http://github.com/hypernumbers/literate-erlang

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

    Integer        <---> Double
                         ! Erlang supports big nums
                         Also Erlang supports radix of 2..36 check in js
    Float          <---> Double
    Char           <---> Double
    String         <---> Array of UTF16 character points
                         All fn calls out to external fns will be cast on return
                         Is this sensible or should be try and use native strings?
                         Would we need to wrap a cast around all list operations?
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

General Overview
----------------

The Luvviescript toolchain compiles Erlang to Javascript in a three step process:
* normal Erlang syntax tools convert the Erlang to a standard Erlang AST
* the core luvviescript libraries decorate that AST with line/column/file information and then transpose it to the Javascript AST defined by the Mozilla Parser API (ADD REF)
* standard Javascript tools (emcodegen.js and sourcemap.js) convert that Javascript AST to runnable Javascript and a Source Mapping

The substantial development work in this libraries is therefore the AST to AST transformation.

The Production ToolChain
------------------------

The production toolchain uses normal Erlang syntax tools to process and validate the Erlang onto a standard Erlang Abstract Syntax tree. That tree is deficient for our purposes:
* each element is only decorated with the **line number** in which it occurs in the original code whereas the source mapping files need **column information** as well.

In order to generate the appropriate Source Map info the following process is implemented:
* the raw Erlang is normalised by Erlang preprocessor to the ``.P`` form - this is a standard format with reliable indenting - macros are expanded and include files are incorporated
* this format is not quite suitable for our purposes because it contains additional lines that screw up the AST so we filter them out give a closely related form call ``.P2``

At this stage we are ready to start building a version of the AST that contains both line and column information which we need for source maps. Two transformations are performed on the ``.P2`` format
* it is compiled to the standard Erlang Abstract Syntax tree (AST) format
* it is tokenised to a format we call ``.tks`` with standard Erlang syntax tools - in a token format that preserves comments and whitespace in the token stream - again the tokens contain only line information
* the token stream is then traversed to accumulate the text length which is used to make a line/column tuple version of the token stream (the ``.tks2`` format)
* these tokens are then collected to make a dictionary which can be merged with the ``.ast`` format to make the full line/column Erlang AST - the ``.ast2`` format

This AST is then fit to be transformed into Javascript.

The Production Toolchain - Another View
---------------------------------------

THIS SECTION NEEDS REWRITING TO MAKE IT UP TO DATE

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

Once this has been transformed so the line numbers have been replaced by a tuple of file name, line no and column no, it can be **translated** into the Javascript AST.

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

Help With The Javascript Parser API AST
=======================================

The best way to explore the Javascript Parser API AST (which is the target of the luvvie script compiler) is to use the online Esprima javascript parser which is available online at:
http://esprima.org/demo/parse.html

This takes javascript and compiles it down to the Parser API.

LuvvieScript itself passes the Parser API into the Escodegen for the reverse journey.

https://github.com/Constellation/escodegen

Contributing
============

TIS SECTION NEEDS TO BE REWRITTEN TO BE UP TO DATE WITH THE USE OF LITERATE JAVASCRIPT

The contribution cycle is this:

* read the issues list at https://github.com/hypernumbers/LuvvieScript/issues
* find a file in ``test/supported`` that is not making the appropriate productions and which nobody has claimed (or write one that addresses something none of the existing ones do)
* raise an issue of type ``enhancement`` against the test file name
* fix and raise a pull request

The test files are compile with a rebar plugin.

You run this plugin with the command:

```erlang
    rebar compile
    rebar make_luvv
```

``rebar make_luvv`` compiles all the erlang files in ``test/passing/src`` and ``test/not_passing/src`` into the corresponding ``.js`` files in ``test/*/js``

At the moment, of course, these are only fragement files.

Anyone who has a pull request accepted will get a **magnificent** LuvvieScript committers t-shirt comme ça:

<img src='https://raw.github.com/hypernumbers/LuvvieScript/master/priv/images/gordonguthrie.jpg' />


If You Have Read This Far
=========================

Follow me on Twitter @gordonguthrie

If you want to be kept in touch then star this repo on github

But mostly, fork this repo and get hacking

