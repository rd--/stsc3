// () -> (() -> int)
function makeCounter() {
    var x = 0;
    function f() {
        x = x + 1;
        return x;
    }
    return f;
}

// () -> int
var ugenCounter = makeCounter();

class UGen {
    constructor(name, param, mce, nc, rt, op, inputs) {
        this.ugenName = name; // str
        this.paramNames = param; // [str]
        this.mceNames = mce; // [str]
        this.numChan = nc; // int
        this.ugenRate = rt; // str | [int]
        this.specialIndex = op; // maybe int
        this.ugenId = ugenCounter(); // int
        this.inputValues = inputs; // [ugen | [ugen]]
    }
}

class Port {
    constructor(ugen, index) {
        this.ugen = ugen; // ugen
        this.index = index; // int
    }
}

// ([], int) -> * ; atWrap([1, 2, 3], 5) == 3
function atWrap(array, index) { return array[index % array.length]; }

// ([], int) -> [] ; extendCyclically([1, 2, 3], 8)
function extendCyclically(array, size) {
    var k = array.length;
    var result = array.slice(0, k)
    for(let x = 0; x < size - k; x += 1) {
        result.push(atWrap(array, x));
    }
    return result;
}

// [num] -> num ; maxItem([1, 2, 3, 4, 3, 2, 1])
function maxItem(array) {
    var f = (i, j) => Math.max(i, j);
    return array.reduce(f);
}

// [] -> bool ; containsArray([1, 2, [3, 4]])
function containsArray(array) {
    return array.some(item => Array.isArray(item));
}

// [[]] -> [[]] ; extendToBeOfEqualSize([[1, 2], [3, 4, 5]]) ; extendToBeOfEqualSize([[440, 550], 0])
function extendToBeOfEqualSize(array) {
    var m = maxItem(array.map(item => Array.isArray(item) ? item.length : 1));
    return array.map(item => extendCyclically(Array.isArray(item) ? item : [item], m));
}

// [[]] -> [[]] ; transpose([[1, 2, 3], [4, 5, 6]])
function transpose(array) {
    return array[0].map((col, i) => array.map(row => row[i]));
}

// (int, int -> *) -> [*] ; arrayFill(5, i => i * i)
function arrayFill(k, f) {
    var r = [];
    for(let i = 0; i < k; i++ ) {
        r.push(f(i));
    }
    return r;
}

function makeUGen(name, param, mce, nc, rt, op, inputs) {
    if(containsArray(inputs)) {
        return transpose(extendToBeOfEqualSize(inputs)).map(item => makeUGen(name, param, mce, nc, rt, op, item));
    } else {
        var u = new UGen(name, param, mce, nc, rt, op, inputs);
        return nc == 1 ? u : arrayFill(nc, i => new Port(u, i));
    }
}

function SinOsc(freq, phase) {
    return makeUGen('SinOsc', ['freq', 'phase'], [], 1, 'ar', 0, [freq, phase]);
}

function BinaryOp(ix, a, b) {
    return makeUGen('BinaryOpUGen', ['a', 'b'], [], 1, [0, 1], ix, [a, b]);
}

function Add(a, b) { return BinaryOp(0, a, b); }
function Sub(a, b) { return BinaryOp(1, a, b); }
function Mul(a, b) { return BinaryOp(2, a, b); }

function Pan2(input, pos, level) {
    return makeUGen('Pan2', ['in', 'pos', 'level'], [], 2, 'ar', 0, [input, pos, level]);
}

function PinkNoise() {
    return makeUGen('PinkNoise', [], [], 1, 'ar', 0, []);
}

/*
Mul(SinOsc(440, 0), 0.1)
Pan2(SinOsc(440, 0), 0, 0.1)
Mul(SinOsc([440, 441], 0), 0.1)
Mul(PinkNoise(), 0.1)
*/
