/*
 * This javascript library corresponds to the Erlang module in
 * pure Erlang
 *
 * LuvvieScript implements only a SUBSET of Erlang and this module
 * reflects that
 */
var erlang = (function() {

    var internalfns = {}, exportedfns = {};

    // Internal Fns
    internalfns.is = function(Obj, Type) {
        if (("type" in Obj) && (Obj.type() === Type)) {
            return true;
        } else {
            return false;
        };
    };

    exportedfns.is_int = function(Integer) {
        return internalfns.is(Integer, "int");
    };

    exportedfns.is_float = function(Float) {
        return internalfns.is(Float, "float");
    };

    exportedfns.is_atom = function(Atom) {
        return internalfns.is(Atom, "atom");
    };

    exportedfns.is_tuple = function(Tuple) {
        return internalfns.is(Tuple, "tuple");
    };

    exportedfns.is_list = function(List) {
        return internalfns.is(List, "list");
    };

    return exportedfns;
})()