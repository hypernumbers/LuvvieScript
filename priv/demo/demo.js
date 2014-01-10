var exports = {};
exports.test = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        return test();
        break;
    default:
    }
};
first = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        return 1;
        break;
    default:
        return 'throw error';
    }
};
second = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        return 2;
        break;
    default:
        return 'throw error';
    }
};
test = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        var A;
        var B;
        var C;
        var cor3;
        A = first();
        B = second();
        C = third();
        cor3 = B / C;
        return A + cor3;
        break;
    default:
        return 'throw error';
    }
};
third = function () {
    _args = arguments.length;
    switch (_args) {
    case 0:
        return 3;
        break;
    default:
        return 'throw error';
    }
};
//@ sourceMappingURL=demo.js.map