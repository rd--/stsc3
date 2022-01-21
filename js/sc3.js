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

class Ugen {
    constructor(name, param, mce, nc, rt, op, inputs) {
        this.ugenName = name; // str
        this.paramNames = param; // [str]
        this.mceNames = mce; // [str]
        this.numChan = nc; // int2
        this.ugenRate = rt; // str
        this.specialIndex = op; // maybe int
        this.ugenId = ugenCounter(); // int
        this.inputValues = inputs; // [input | [input]] where input : number | port
    }
}

function isUgen(obj) {
    return obj.constructor === Ugen;
}

class Port {
    constructor(ugen, index) {
        this.ugen = ugen; // ugen
        this.index = index; // int
    }
}

function isPort(obj) {
    return obj.constructor === Port;
}

// ([], int) -> * ; atWrap([1, 2, 3], 5) == 3
function atWrap(array, index) {
    return array[index % array.length];
}

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

function arrayReplicate(k, v) {
    var r = [];
    for(let i = 0; i < k; i++ ) {
        r.push(v);
    }
    return r;
}

function makeUgen(name, param, mce, nc, rt, op, inputs) {
    if(containsArray(inputs)) {
        return transpose(extendToBeOfEqualSize(inputs)).map(item => makeUgen(name, param, mce, nc, rt, op, item));
    } else {
        var u = new Ugen(name, param, mce, nc, rt, op, inputs);
        switch(nc) {
            case 0: return (new Port(u, null));
            case 1: return (new Port(u, 0));
            default: arrayFill(nc, i => new Port(u, i));
        }
    }
}

function SinOsc(freq, phase) {
    return makeUgen('SinOsc', ['freq', 'phase'], [], 1, 'ar', 0, [freq, phase]);
}

function calcRateCode(r) {
    switch(r) {
        case 'ir': return 0;
        case 'kr': return 1;
        case 'ar': return 2;
        default: console.error('calcRateCode', r); return -1;
    }
}

function maxRateOf(array) {
    var f = (i, j) => (calcRateCode(i) > calcRateCode(j)) ? i : j;
    console.log('maxRateOf', array);
    return array.reduce(f);
}

function inputRate(i) {
    return Array.isArray(i) ? maxRateOf(i.map(inputRate)) : (isPort(i) ? i.ugen.ugenRate : 'ir');
}

function deriveRate(i) {
    return maxRateOf(i.map(inputRate));
}

function BinaryOp(ix, a, b) {
    return makeUgen('BinaryOpUgen', ['a', 'b'], [], 1, deriveRate([a, b]), ix, [a, b]);
}

function Add(a, b) { return BinaryOp(0, a, b); }
function Sub(a, b) { return BinaryOp(1, a, b); }
function Mul(a, b) { return BinaryOp(2, a, b); }

function Pan2(input, pos, level) {
    return makeUgen('Pan2', ['in', 'pos', 'level'], [], 2, 'ar', 0, [input, pos, level]);
}

function PinkNoise() {
    return makeUgen('PinkNoise', [], [], 1, 'ar', 0, []);
}

function inputAsArray(i) {
    return Array.isArray(i) ? i : [i];
}

function Out(bus, channelsArray) {
    var inputArray = [bus].concat(inputAsArray(channelsArray));
    return makeUgen('Out', ['bus'], ['channelsArray'], 0, deriveRate(inputArray), 0, inputArray);
}

function isNumber(x) {
    return (typeof x === 'number');
}

// p : port | [port], array : [number | ugen] ; traverse graph from p adding leaf nodes to a
function ugenTraverseCollecting(p, array) {
    if(Array.isArray(p)) {
        p.forEach(item => ugenTraverseCollecting(item, array));
    } else {
        array.push(p.ugen);
        p.ugen.inputValues.forEach(item => isNumber(item) ? array.push(item)  : ugenTraverseCollecting(item, array));
    }
}

// all leaf nodes, in sequence, may contain duplicate entries
function ugenGraphLeafNodes(graph) {
    var array = [];
    ugenTraverseCollecting(graph, array);
    return array.reverse();
}

// [*] -> [*] ; delete duplicate entries, retain ordering ; arrayNub([1, 2, 3, 2, 1, 2, 3, 4, 3, 2, 1])
function arrayNub(array) {
    return array.filter((item, index) => array.indexOf(item) === index);
}

class Graph {
    constructor(name, graph) {
        var leafNodes = ugenGraphLeafNodes(graph);
        var ugenSeq = leafNodes.filter(item => isUgen(item));
        this.graphName = name;
        this.ugenSet = new Set(ugenSeq);
        this.ugenIdSeq = arrayNub(ugenSeq.map(item => item.ugenId));
        this.constantSeq = arrayNub(leafNodes.filter(item => isNumber(item)));
    }
}

function isGraph(obj) {
    return obj.constructor === Graph;
}

function graphConstantIndex(g, k) {
    return g.constantSeq.indexOf(k);
}

function graphUgenIndex(g, u) {
    return g.ugenIdSeq.findIndex(item => item === u.ugenId);
}

function graphInputSpec(g, i) {
    return isPort(i) ? [graphUgenIndex(g, i.ugen), i.index] : [-1, graphConstantIndex(g, i)];
}

function graphUgenSpec(g, u) {
    console.log(
        u.ugenName,
        calcRateCode(u.ugenRate),
        u.inputValues.length,
        u.numChan,
        u.specialIndex,
        u.inputValues.map(i => graphInputSpec(g, i)),
        arrayReplicate(u.numChan, calcRateCode(u.ugenRate))
    );
}

function graphSyndef(g) {
    console.log(g.constantSeq);
    g.ugenSet.forEach(item => graphUgenSpec(g, item));
}

/*
u = Mul(SinOsc(440, 0), 0.1)
u = Pan2(SinOsc(440, 0), 0, 0.1)
u = Mul(SinOsc([440, 441], 0), 0.1)
u = Mul(PinkNoise(), 0.1)

*/

u = Out(0, Mul(SinOsc([440, 441], 0), 0.1));
g = new Graph('g', u);
graphSyndef(g);
