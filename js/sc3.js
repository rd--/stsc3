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

// Pseudo

function sum(a) {
    return a.reduce(add);
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
