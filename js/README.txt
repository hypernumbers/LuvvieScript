PURPOSE
-------

To convert Erlang types to Javascript ones (and back)

BUILDING TERMS
--------------

The term file in js/terms/terms.log is built by running the erlang function bert_utils:all/0

This term file (with a slight edit) is wheeched into the javascript.

HOWTO USE
---------

Make a symlink to the bert.js library by running the command
ln -s ../deps/rusty.io/bert.js ./bert.js

The open the page convert.html in a browser and it will run the module type_conversion.js
