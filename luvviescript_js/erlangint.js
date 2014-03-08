/*
 * a naive implementation of integers for LuvvieScript
 * this does run time type checks
 *
 * might be switched to compile time casting later on
 */

function erlangint () {
    this.list = arguments;
}

erlangint.prototype.type = function () {
    return 'int';
}

erlangint.prototype.external_format = function() {
    var Json = {"int": this.value}
    return JSON.stringify(Json);
}