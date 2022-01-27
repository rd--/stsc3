// Counter

// () -> (() -> int)
function makeCounter() {
    var x = 0;
    function f() {
        x = x + 1;
        return x;
    }
    return f;
}

// String

function isString(x) { return typeof x == 'string'; }

// Number

function isNumber(x) {
    return (typeof x === 'number');
}

var pi = Math.PI;

var inf = Infinity;

function randomInteger(min, max) {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min) + min); // the maximum is exclusive and the minimum is inclusive
}

// Object

function objectKeyFromValue(object, value) {
  return Object.keys(object).find(key => object[key] === value);
}

// Uint8Array

function isUint8Array(x) {
    return (x instanceof Uint8Array);
}

// Encode

function encodeUsing(k, f) {
    var b = new ArrayBuffer(k);
    f(new DataView(b));
    return new Uint8Array(b);
}

Number.prototype.encodeUint8 = function() {
    return encodeUsing(1, b => b.setUint8(0, this));
}

Number.prototype.encodeInt8 = function() {
    return encodeUsing(1, b => b.setInt8(0, this));
}

Number.prototype.encodeInt16 = function() {
    return encodeUsing(2, b => b.setInt16(0, this));
}

Number.prototype.encodeInt32 = function() {
    return encodeUsing(4, b => b.setInt32(0, this));
}

// Number(1.0).encodeFloat32() //=> [63, 128, 0, 0]
Number.prototype.encodeFloat32 = function() {
    return encodeUsing(4, b => b.setFloat32(0, this));
}

// 'string'.encodePascalString() //=> [6, 115, 116, 114, 105, 110, 103]
String.prototype.encodePascalString = function () {
    var k = this.length;
    var e = new Uint8Array(k + 1);
    e[0] = k;
    for(var i = 1; i < k + 1; i++) {
        e[i] = this.charCodeAt(i - 1);
    }
    return e;
}

// Flatten a tree of arrays of Uint8Array to an array
function flattenByteEncodingTo(e, a) {
    if(isUint8Array(e)) {
        e.forEach(item => a.push(item));
    } else if(Array.isArray(a)) {
        e.forEach(item => flattenByteEncodingTo(item, a));
    } else {
        console.error("flattenByteEncodingTo", e);
    }
}

function flattenByteEncoding(e) {
    var a = []
    flattenByteEncodingTo(e, a);
    return new Uint8Array(a);
}

// Ugen

// () -> int
var ugenCounter = makeCounter();

class Ugen {
    constructor(name, nc, rt, op, inputs) {
        this.ugenName = name; // str
        this.numChan = nc; // int
        this.ugenRate = rt; // str
        this.specialIndex = op; // maybe int
        this.ugenId = ugenCounter(); // int
        this.inputValues = inputs; // [number | port]
        this.mrg = [];
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
    return obj instanceof Port;
}

function deriveRate(rt, inputs) {
    return isNumber(rt) ? rt : inputs.atIndices(rt).map(inputRate).maxItem();
}

// If rt is a scalar it is the operating rate, if it is an array it is indices into the inputs telling how to derive the rate.
function makeUgen(name, nc, rt, op, inputs) {
    //console.log('makeUgen', name, nc, rt, op, inputs);
    if(inputs.containsArray()) {
        return inputs.extendToBeOfEqualSize().transpose().map(item => makeUgen(name, nc, rt, op, item));
    } else {
        var u = new Ugen(name, nc, deriveRate(rt, inputs), op, inputs);
        switch(nc) {
            case 0: return (new Port(u, null));
            case 1: return (new Port(u, 0));
            default: return arrayFill(nc, i => new Port(u, i));
        }
    }
}

Ugen.prototype.displayName = function() {
    switch(this.ugenName) {
    case 'UnaryOpUGen': return objectKeyFromValue(unaryOperators, this.specialIndex);
    case 'BinaryOpUGen': return objectKeyFromValue(binaryOperators, this.specialIndex);
    default: return this.ugenName;
    }
}

// Rate

var Rate = {ir: 0, kr: 1, ar: 2, dr: 3}

function rateSelector(r) {
    return objectKeyFromValue(Rate, r);
}

// Input = Port | Number

function inputRate(i) {
    //console.log('inputRate', i);
    return isPort(i) ? i.ugen.ugenRate : (isNumber(i) ? Rate.ir : console.error('inputRate', i));
}

// Mrg

// inputFirstUgen([SinOsc([440, 441], 0), SinOsc(442, 0)])
function inputFirstUgen(i) {
    if(Array.isArray(i)) {
        //console.log('inputFirstUgen: array', i)
        return i.find(inputFirstUgen).ugen || null;
    } else if(isPort(i)) {
        //console.log('inputFirstUgen: port', i)
        return i.ugen;
    } else {
        //console.log('inputFirstUgen: number?', i)
        return null;
    }
}

function mrg(lhs,rhs) {
    var u = inputFirstUgen(lhs);
    //console.log('mrg', lhs, rhs, u);
    u ? (Array.isArray(rhs) ? rhs.forEach(item => u.mrg.push(item)) : u.mrg.push(rhs)) : console.error("mrg?");
    return lhs;
}

// Kr

function krMutateInPlace(i) {
    if(isPort(i)) {
        // console.log('kr: port', i);
        krMutateInPlace(i.ugen);
    } else if(isUgen(i)) {
        // console.log('kr: ugen', i);
        i.ugenRate = i.ugenRate === 2 ? 1 : i.ugenRate;
        i.inputValues.forEach(item => krMutateInPlace(item))
    } else if(Array.isArray(i)) {
        // console.log('kr: array', i);
        i.forEach(item => krMutateInPlace(item))
    } else {
        if(!isNumber(i)) {
            console.error('krMutateInPlace', i);
        }
    }
}

function kr(i) { krMutateInPlace(i); return i; }

// Operators

function UnaryOpWithConstantOptimiser(ix, a) {
    if(isNumber(a)) {
        switch(ix) {
        case 0: return 0 - a;
        case 5: return Math.abs(a);
        case 8: return Math.ceil(a);
        case 9: return Math.floor(a);
        case 12: return a * a;
        case 13: return a * a * a;
        case 14: return Math.sqrt(a);
        case 16: return 1 / a;
        case 28: return Math.sin(a);
        case 29: return Math.cos(a);
        case 30: return Math.tan(a);
        }
    }
    return makeUgen('UnaryOpUGen', 1, [0], ix, [a]);
}

// [1, [], [1], [1, 2], [1, null], SinOsc(440, 0), [SinOsc(440, 0)]].map(isArrayConstant)
function isArrayConstant(a) {
    return Array.isArray(a) && a.every(isNumber);
}

function UnaryOp(ix, a) {
    if(isArrayConstant(a)) {
        return a.map(item => UnaryOpWithConstantOptimiser(ix, item));
    } else {
        return UnaryOpWithConstantOptimiser(ix, a);
    }
}

function BinaryOpWithConstantOptimiser(ix, a, b) {
    if(isNumber(a) && isNumber(b)) {
        switch(ix) {
        case 0: return a + b;
        case 1: return a - b;
        case 2: return a * b;
        case 4: return a / b;
        }
    }
    return  makeUgen('BinaryOpUGen', 1, [0, 1], ix, [a, b]);
}

function BinaryOp(ix, a, b) {
    if(Array.isArray(a) || Array.isArray(b)) {
        var expanded = [unitArrayIfScalar(a), unitArrayIfScalar(b)].extendToBeOfEqualSize().transpose();
        // console.log('BinaryOp: array constant', expanded);
        return expanded.map(item => BinaryOpWithConstantOptimiser(ix, item[0], item[1]));
    } else {
        return BinaryOpWithConstantOptimiser(ix, a, b);
    }
}

// Null

function isNull(x) { return x === null; }

function isUndefined(x) { return x === undefined; }

function nullFix(message, inputValue, defaultValue) {
    if(isNull(inputValue) || isUndefined(inputValue)) {
        console.warn('nullFix', message, inputValue, defaultValue);
        return defaultValue;
    } else {
        return inputValue;
    }
}

// Smalltalk

/*
append([1, 2, 3], [4, 5]) //=> [1, 2, 3, 4, 5]
*/
function sum(a) { return a.reduce(add); }
function product(a) { return a.reduce(mul); }
function collect(array, proc) { return array.map(proc); }
function dup(proc, count) { return arrayFill(nullFix('dup: count?', count, 2), proc); }
function timesRepeat(count, proc) { for(i = 0; i < count; i++) { proc(); }; }
function append(lhs, rhs) { return lhs.concat(rhs); }
function transpose(array) { return array.transpose(); }
function reverse(array) { return array.reverse(); }
function concatenation(array) { return array.concatenation(); }
function clump(array, n) { return array.clump(n); }
function mean(array) { return fdiv(sum(array), array.length); }
function choose(array) { return array[randomInteger(0, array.length)]; }
function nth(array, index) { return array[index - 1]; }
function size(array) { return array.length; }
function to(from, to) { return arrayFromTo(from, to); }
function first(array) { return array[0]; }
function second(array) { return array[1]; }
function third(array) { return array[2]; }
function roundTo(a, b) { return round(a, b); }
function rounded(a) { return round(a, 1); }
function reciprocal(a) { return recip(a); }
function negated(a) { return neg(a); }
function truncateTo(a, b) { return trunc(a, b); }

// Env

var EnvDict = {
    step: 0,
    lin: 1, linear: 1,
    exp: 2, exponential: 2,
    sin: 3, sine: 3,
    wel: 4, welch: 4,
    sqr: 6, squared: 6,
    cub: 7, cubed: 7,
    hold: 8
};

class EnvSpec {
    constructor(levels, times, curves, releaseNode, loopNode, offset) {
        this.levels = levels;
        this.times = times;
        this.curves = Array.isArray(curves) ? curves : [curves];
        this.releaseNode = releaseNode;
        this.loopNode = loopNode;
        this.offset = offset;
        //console.log('EnvSpec', curves, this);
    }
}

// Env([0, 1, 0], [0.1, 0.9], 'lin', null, null, 0).coord().shallowEq([0, 2, -99, -99, 1, 0.1, 1, 0, 0, 0.9, 1, 0])
function Env(levels, times, curves, releaseNode, loopNode, offset) {
    return new EnvSpec(levels, times, curves, releaseNode, loopNode, offset);
}

EnvSpec.prototype.coord = function() {
    var n = this.levels.length - 1;
    var r = [];
    r.push(this.levels[0]);
    r.push(n);
    r.push(this.releaseNode || -99);
    r.push(this.loopNode || -99);
    for(i = 0; i < n; i++) {
        var c = this.curves.atWrap(i);
        r.push(this.levels[i + 1]);
        r.push(this.times.atWrap(i));
        r.push(EnvDict[c] || 5);
        r.push(isString(c) ? 0 : c);
    }
    return r;
}

function EnvADSR(attackTime, decayTime, sustainLevel, releaseTime, peakLevel, curve) {
    return Env(
        [0, peakLevel, mul(peakLevel, sustainLevel), 0],
        [attackTime, decayTime, releaseTime],
        curve,
        2,
        null,
        0);
}

function ADSR(gate, attackTime, decayTime, sustainLevel, releaseTime, curve) {
    var env = EnvADSR(attackTime, decayTime, sustainLevel, releaseTime, 1, curve);
    return EnvGen(gate, 1, 0, 1, 0, env.coord());
}

function EnvASR(attackTime, sustainLevel, releaseTime, curve) {
    return Env(
        [0, sustainLevel, 0],
        [attackTime, releaseTime],
        curve,
        1,
        null,
        0);
}

function ASR(gate, attackTime, releaseTime, curve) {
    var env = EnvASR(attackTime, 1, releaseTime, curve);
    return EnvGen(gate, 1, 0, 1, 0, env.coord());
}

// Texture

function OverlapTexture(graphFunc, sustainTime, transitionTime, overlap) {
        return sum(to(0, overlap - 1).map(function(i) {
            var trg = kr(Impulse(1 / (sustainTime + (transitionTime * 2)), i / overlap));
            var snd = graphFunc(trg);
            var env = Env([0, 1, 1, 0], [transitionTime,sustainTime,transitionTime], 'sin', null, null, 0);
            var sig = mul(snd, EnvGen(trg, 1, 0, 1, 0, env.coord()));
            //console.log('OverlapTexture', trg, snd, env, sig);
            return sig;
        }));
}

// Graph

// p : port | [port], c & w : {number | ugen} ; traverse graph from p adding leaf nodes to the set c ; w protects from loops in mrg
function ugenTraverseCollecting(p, c, w) {
    if(Array.isArray(p)) {
        //console.log('ugenTraverseCollecting: array', p);
        p.forEach(item => ugenTraverseCollecting(item, c, w));
    } else if(isPort(p)) {
        //console.log('ugenTraverseCollecting: port', p);
        if(!w.has(p.ugen)) {
            c.add(p.ugen);
            p.ugen.inputValues.forEach(item => isNumber(item) ? c.add(item)  : ugenTraverseCollecting(item, c, w));
            p.ugen.mrg.forEach(item => isNumber(item) ? c.add(item) : ugenTraverseCollecting(item, c, c));
        }
    } else {
        console.error('ugenTraverseCollecting', p, c, w);
    }
}

// all leaf nodes of p
function ugenGraphLeafNodes(p) {
    var c = new Set();
    ugenTraverseCollecting(p, c, new Set());
    return Array.from(c);
}

// ugens are sorted by id, which is in applicative order. a maxlocalbufs ugen is always present.
class Graph {
    constructor(name, graph) {
        var leafNodes = ugenGraphLeafNodes(graph);
        var ugens = leafNodes.filter(item => isUgen(item)).sort((i, j) => i.ugenId - j.ugenId);
        var constants = leafNodes.filter(item => isNumber(item));
        var numLocalBufs = ugens.filter(item => isUgen(item) && item.ugenName == 'LocalBuf').length;
        this.graphName = name;
        this.ugenSeq = [MaxLocalBufs(numLocalBufs).ugen].concat(ugens);
        this.constantSeq = [numLocalBufs].concat(constants).nub().sort((i, j) => i - j);
    }
}

function isGraph(obj) {
    return obj.constructor === Graph;
}

Graph.prototype.constantIndex = function(k) {
    return this.constantSeq.indexOf(k);
}

// lookup ugen index at graph given ugenId
Graph.prototype.ugenIndex = function(k) {
    return this.ugenSeq.findIndex(u => u.ugenId === k);
}

// port|num -> [int, int]
Graph.prototype.inputSpec = function(i) {
    return isPort(i) ? [this.ugenIndex(i.ugen.ugenId), i.index] : [-1, this.constantIndex(i)];
}

Graph.prototype.printUgenSpec = function(u) {
    console.log(
        u.ugenName,
        u.ugenRate,
        u.inputValues.length,
        u.numChan,
        u.specialIndex,
        u.inputValues.map(i => this.inputSpec(i)),
        arrayReplicate(u.numChan, u.ugenRate)
    );
}

var SCgf = Number(1396926310);

Graph.prototype.printSyndef = function() {
    console.log(SCgf, 2, 1, this.graphName, this.constantSeq.length, this.constantSeq, 0, [], 0, [], this.ugenSeq.length);
    this.ugenSeq.forEach(item => this.printUgenSpec(item));
    console.log(0, []);
}

Graph.prototype.encodeUgenSpec = function(u) {
    return [
        u.ugenName.encodePascalString(),
        Number(u.ugenRate).encodeInt8(),
        Number(u.inputValues.length).encodeInt32(),
        Number(u.numChan).encodeInt32(),
        Number(u.specialIndex).encodeInt16(),
        u.inputValues.map(i => this.inputSpec(i).map(ix => Number(ix).encodeInt32())),
        arrayReplicate(u.numChan, Number(u.ugenRate).encodeInt8())
    ];
}

Graph.prototype.encodeSyndef = function() {
    return flattenByteEncoding([
        SCgf.encodeInt32(),
        Number(2).encodeInt32(), // file version
        Number(1).encodeInt16(), // # synth definitions
        this.graphName.encodePascalString(), // pstring
        Number(this.constantSeq.length).encodeInt32(),
        this.constantSeq.map(item => Number(item).encodeFloat32()),
        Number(0).encodeInt32(), // # param
        Number(0).encodeInt32(), // # param names
        Number(this.ugenSeq.length).encodeInt32(),
        this.ugenSeq.map(item => this.encodeUgenSpec(item)),
        Number(0).encodeInt16() // # variants
    ]);
}

// Print

function printSyndefOf(u) {
    var g = new Graph('sc3.js', Out(0, u));
    g.printSyndef(g);
}

// Pretty print

Graph.prototype.inputDisplayName = function(i) {
    if(isPort(i)) {
        var id = String(this.ugenIndex(i.ugen.ugenId));
        var nm = i.ugen.displayName();
        var ix = i.ugen.numChan > 1 ? ('[' + String(i.index) + ']') : '';
        return id + '_' + nm + ix;
    } else if(isNumber(i)) {
        return String(i);
    } else {
        console.error('inputDisplayName', i);
    }
}

Graph.prototype.prettyPrintUgen = function(u) {
    console.log(
        this.ugenIndex(u.ugenId) + '_' + u.displayName(),
        rateSelector(u.ugenRate),
        '[' + String(u.inputValues.map(i => this.inputDisplayName(i))) + ']'
    );
}

Graph.prototype.prettyPrintSyndef = function() {
    this.ugenSeq.forEach(item => this.prettyPrintUgen(item));
}

function prettyPrintSyndefOf(u) {
    var g = new Graph('sc3.js', Out(0, u));
    g.prettyPrintSyndef(g);
}

// Server commands (Open Sound Control)

function d_recv(syndefArray) {
    return {address: '/d_recv', args: [{type: 'b', value: syndefArray}]};
}

function d_recv_then(syndefArray, onCompletion) {
    return {address: '/d_recv', args: [{type: 'b', value: syndefArray}, {type: 'b', value: onCompletion}]};
}

function s_new0(name, id, addAction, target) {
    return {address: '/s_new', args: [{type: 's', value: name}, {type: 'i', value: id}, {type: 'i', value: addAction}, {type: 'i', value: target}]};
}

function g_freeAll1(id) {
    return {address: '/g_freeAll', args: [{type: 'i', value: id}]};
}
