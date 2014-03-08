/*
 * The implementation of erlang tuples as a Javascript Class
 * Thi is a very early and rough implentation to get an end-to-end
 * working system
 *
 * It is *probably* NOT how it will end up being
 */

function erlangtuple () {
    this.value = arguments;
}

erlangtuple.prototype.type = function () {
    return 'tuple';
}

erlangtuple.prototype.external_format = function() {
    var Json = {"tuple": this.value}
    return JSON.stringify(Json);
}