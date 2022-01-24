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

// Number

function isNumber(x) {
    return (typeof x === 'number');
}

var pi = Math.PI;

function randomInteger(min, max) {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min) + min); // the maximum is exclusive and the minimum is inclusive
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

function makeUgen(name, nc, rt, op, inputs) {
    if(inputs.containsArray()) {
        return inputs.extendToBeOfEqualSize().transpose().map(item => makeUgen(name, nc, rt, op, item));
    } else {
        var u = new Ugen(name, nc, rt, op, inputs);
        switch(nc) {
            case 0: return (new Port(u, null));
            case 1: return (new Port(u, 0));
            default: return arrayFill(nc, i => new Port(u, i));
        }
    }
}

// Rate

const Rate = {ir: 0, kr: 1, ar: 2, dr: 4}

// Input = Port | Num | [Input]

function inputRate(i) {
    return Array.isArray(i) ? i.map(inputRate).maxItem() : (isPort(i) ? i.ugen.ugenRate : Rate.ir);
}

function inputAsArray(i) {
    return Array.isArray(i) ? i : [i];
}

// Operators

function UnaryOp(ix, a) {
    return makeUgen('UnaryOpUGen', 1, inputRate([a]), ix, [a]);
}

function BinaryOp(ix, a, b) {
    return makeUgen('BinaryOpUGen', 1, inputRate([a, b]), ix, [a, b]);
}

// Pseudo Ugens

function sum(a) {
    return a.reduce(add);
}

function Splay(inArray, spread, level, center, levelComp) {
    var n = Math.max(2, inArray.length);
    var positions = arrayFromTo(0, n - 1).map(item => MulAdd(sub(mul(item, fdiv(2, sub(n, 1))), 1), spread, center));
    return sum(Pan2(inArray, positions, mul(level, levelComp ? Math.sqrt(1 / n) : 1)));
}

function Splay2(inArray) {
    var n = Math.max(2, inArray.length);
    var positions = arrayFromTo(0, n - 1).map(item => item * (2 / (n - 1)) - 1);
    return sum(Pan2(inArray, positions, Math.sqrt(1 / n)));
}

function LinLin(input, srclo, srchi, dstlo, dsthi) {
    var scale  = (dsthi - dstlo) / (srchi - srclo);
    var offset = dstlo - (scale * srclo);
    return MulAdd(input, scale, offset);
}

function InFb(numChannels, bus) {
    return InFeedback(numChannels, bus);
}

function Select2(predicate, ifTrue, ifFalse) {
    return (predicate * (ifTrue - ifFalse)) + ifFalse;
}

function TChoose(trig, array) {
    return Select(TIRand(0, array.length - 1, trig), array);
}

function PMOsc(carfreq, modfreq, pmindex, modphase) {
    return SinOsc(carfreq, SinOsc(modfreq, modphase, pmindex));
}

function XLn(start, end, dur) {
    return XLine(start, end, dur, 0);
}

// Smalltalk

function collect(array, proc) { return array.map(proc); }
function dup(proc, count) { return arrayFill(count, proc); }
function append(lhs, rhs) { return lhs.concat(rhs); }
function transpose(array) { return array.transpose(); }
function reverse(array) { return array.reverse(); }
function mean(array) { return fdiv(sum(array), array.length); }
function choose(array) { return array[randomInteger(0, array.length)]; }
function to(from, to) { return arrayFromTo(from, to); }
function first(array) { return array[0]; }
function second(array) { return array[1]; }
function third(array) { return array[2]; }
function roundTo(a, b) { return round(a, b); }
function rounded(a) { return round(a, 1); }

// Env

function isString(x) { return typeof x == 'string'; }

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

// Texture

function OverlapTexture(graphFunc, sustainTime, transitionTime, overlap) {
        return to(0, overlap - 1).map(function(i) {
            var trg = Impulse(1 / (sustainTime + (transitionTime * 2)), i / overlap);
            var snd = graphFunc(trg);
            var env = Env([0,1,1,0], [transitionTime,sustainTime,transitionTime], 'sin', null, null, 0);
            var sig = mul(snd, EnvGen(trg, 1, 0, 1, 0, env.coord()));
            console.log(trg, snd, env, sig);
            return sig;
        }).sum;
}

// Graph

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
function ugenGraphLeafNodes(p) {
    var array = [];
    ugenTraverseCollecting(p, array);
    return array.reverse();
}

class Graph {
    constructor(name, graph) {
        var leafNodes = ugenGraphLeafNodes(graph);
        var ugenSeq = leafNodes.filter(item => isUgen(item));
        this.graphName = name;
        this.ugenSet = new Set(ugenSeq);
        this.ugenIdSeq = ugenSeq.map(item => item.ugenId).nub();
        this.constantSeq = leafNodes.filter(item => isNumber(item)).nub();
    }
}

function isGraph(obj) {
    return obj.constructor === Graph;
}

Graph.prototype.constantIndex = function(k) {
    return this.constantSeq.indexOf(k);
}

Graph.prototype.ugenIndex = function(u) {
    return this.ugenIdSeq.findIndex(item => item === u.ugenId);
}

// port|num -> [int, int]
Graph.prototype.inputSpec = function(i) {
    return isPort(i) ? [this.ugenIndex(i.ugen), i.index] : [-1, this.constantIndex(i)];
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

// 'SCgf'
const SCgf = Number(1396926310);

Graph.prototype.printSyndef = function() {
    console.log(SCgf, 2, 1, this.graphName, this.constantSeq.length, this.constantSeq, 0, [], 0, [], this.ugenSet.size);
    this.ugenSet.forEach(item => this.printUgenSpec(item));
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
        Number(this.ugenSet.size).encodeInt32(),
        Array.from(this.ugenSet).map(item => this.encodeUgenSpec(item)),
        Number(0).encodeInt16() // # variants
    ]);
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
