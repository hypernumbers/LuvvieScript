/*
 * a naive implementation of lists for LuvvieScript
 * this list module is 'copy on write' with all the penalties that incurs
 *
 * This module is to get a running system up so that it can be optimised laters
 */

function erlanglist () {
    this.value = arguments;
}

erlanglist.prototype.type = function () {
    return 'list';
}

erlanglist.prototype.external_format = function() {
    var Json = {"list": this.value}
    return JSON.stringify(Json);
}