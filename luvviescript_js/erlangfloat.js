/*
 * a naive implementation of floats for LuvvieScript
 * this does run time type checks
 *
 * might be switched to compile time casting later on
 */

function erlangfloat () {
    this.value = arguments[0];
}

erlangfloat.prototype.type = function () {
    return 'float';
}

erlangfloat.prototype.external_format = function() {
    var Json = {"float": this.value}
    return JSON.stringify(Json);
}