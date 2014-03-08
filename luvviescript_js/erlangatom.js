/*
 * The implementation of erlang atoms as a Javascript Class
 */

// The compiiler will only ever emit a call to this with a single argument
function erlangatom () {
    this.value = arguments[0];
}

erlangatom.prototype.type = function () {
    return 'atom';
}

erlangatom.prototype.external_format = function() {
    var Json = {"atom": this.value}
    return JSON.stringify(Json);
}