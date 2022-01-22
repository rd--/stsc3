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

// Array

// [1, 2, 3].atWrap(5) === 3
Array.prototype.atWrap = function(index) {
    return this[index % this.length];
}

// [1, 2, 3].shallowEq([1, 2, 3]) === true
Array.prototype.shallowEq = function(anArray) {
    if (this === anArray) {
        return true;
    }
    if (!Array.isArray(anArray) || (this.length !== anArray.length)) {
        return false;
    }
    for (var i = 0; i < this.length; i++) {
        if (this[i] !== anArray[i]) {
            return false;
        }
    }
    return true;
}

// [1, 2, [3, [4, 5]]].treeEq([1, 2, [3, [4, 5]]])
Array.prototype.treeEq = function(anArray) {
    if (this === anArray) {
        return true;
    }
    if (!Array.isArray(anArray) || (this.length !== anArray.length)) {
        return false;
    }
    for (var i = 0; i < this.length; i++) {
        if(Array.isArray(this[i])) {
            if (!this[i].treeEq(anArray[i])) {
                return false;
            }
        } else {
            if (this[i] !== anArray[i]) {
                return false;
            }
        }
    }
    return true;
}

// [1, 2, 3].extendCyclically(8).shallowEq([1, 2, 3, 1, 2, 3, 1, 2])
Array.prototype.extendCyclically = function(size) {
    var k = this.length;
    var result = this.slice(0, k)
    for(let x = 0; x < size - k; x += 1) {
        result.push(this.atWrap(x));
    }
    return result;
}

// [1, 2, 3, 4, 3, 2, 1].maxItem() === 4
Array.prototype.maxItem = function() {
    return this.reduce((i, j) => Math.max(i, j));
}

// [1, 2, [3, 4]].containsArray() === true
Array.prototype.containsArray = function() {
    return this.some(item => Array.isArray(item));
}

// [[1, 2], [3, 4, 5]].extendToBeOfEqualSize().treeEq([[1, 2, 1], [3, 4, 5]])
// [[440, 550], 0].extendToBeOfEqualSize().treeEq([[440, 550], [0, 0]])
Array.prototype.extendToBeOfEqualSize = function() {
    var m = this.map(item => Array.isArray(item) ? item.length : 1).maxItem();
    return this.map(item => (Array.isArray(item) ? item : [item]).extendCyclically(m));
}

// [[1, 2, 3], [4, 5, 6]].transpose().treeEq([[1, 4], [2, 5], [3, 6]])
Array.prototype.transpose = function() {
    return this[0].map((col, i) => this.map(row => row[i]));
}

// Delete duplicate entries, retain ordering ; [1, 2, 3, 2, 1, 2, 3, 4, 3, 2, 1].nub().shallowEq([1, 2, 3, 4])
Array.prototype.nub = function() {
    return this.filter((item, index) => this.indexOf(item) === index);
}

// arrayIota(5).shallowEq([0, 1, 2, 3, 4])
function arrayIota(k) {
    var r = [];
    for(let i = 0; i < k; i++ ) {
        r.push(i);
    }
    return r;
}

// arrayFill(5, i => i * i).shallowEq([0, 1, 4, 9, 16])
function arrayFill(k, f) {
    return arrayIota(k).map(f);
}

// arrayReplicate(5, 1).shallowEq([1, 1, 1, 1, 1])
function arrayReplicate(k, v) {
    return arrayIota(k).map(unused => v);
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

// Bindings

function SinOsc(freq, phase) {
    return makeUgen('SinOsc', 1, Rate.ar, 0, [freq, phase]);
}

function UnaryOp(ix, a) {
    return makeUgen('UnaryOpUGen', 1, inputRate([a]), ix, [a]);
}

function Tanh(a) {
    return UnaryOp(36, a);
}

function BinaryOp(ix, a, b) {
    return makeUgen('BinaryOpUGen', 1, inputRate([a, b]), ix, [a, b]);
}

function Add(a, b) { return BinaryOp(0, a, b); }
function Sub(a, b) { return BinaryOp(1, a, b); }
function Mul(a, b) { return BinaryOp(2, a, b); }

function HPF(input, freq) {
    return makeUgen('HPF', 1, inputRate([input]), 0, [input, freq]);
}

function Pan2(input, pos, level) {
    return makeUgen('Pan2', 2, inputRate([input]), 0, [input, pos, level]);
}

function PinkNoise() {
    return makeUgen('PinkNoise', 1, Rate.ar, 0, []);
}

function Out(bus, channelsArray) {
    var inputArray = [bus].concat(inputAsArray(channelsArray));
    return makeUgen('Out', 0, inputRate(inputArray), 0, inputArray);
}

// Pseudo

function Sum(a) {
    return a.reduce(Add);
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

/*
u = Mul(SinOsc(440, 0), 0.1)
u = Pan2(SinOsc(440, 0), 0, 0.1)
u = Mul(SinOsc([440, 441], 0), 0.1)
u = Sum(Pan2(HPF(PinkNoise(), [3000, 11000]), SinOsc([1 / 7, 1 / 13], [0, pi]), 0.1))

o = SinOsc(440, 0);
u = Out(0, Mul([o, o], 0.1));
graphSyndef(new Graph('g', u))

*/

u = Mul(Tanh(SinOsc([440, 441], 0)), Mul(SinOsc([0.1, 0.25], 0), 0.1));
g = new Graph('g', Out(0, u));
g.printSyndef();
d = g.encodeSyndef();
fs.writeFile('/tmp/t.scsyndef', d, err => console.log(err))

// Open Sound Control

function d_recv(syndefArray) {
    return {address: '/d_recv', args: [{type: 'b', value: syndefArray}]};
}

function s_new0(name, id, addAction, target) {
    return {address: '/s_new', args: [{type: 's', value: name}, {type: 'i', value: id}, {type: 'i', value: addAction}, {type: 'i', value: target}]};
}

function g_freeAll1(id) {
    return {address: '/g_freeAll', args: [{type: 'i', value: id}]};
}

// Udp

var osc = require('osc'); // https://github.com/colinbdclark/osc.js/ ; npm i osc

function defaultSc3Udp() {
    return new osc.UDPPort({
        localAddress: '127.0.0.1',
        localPort: null,
        remoteAddress: '127.0.0.1',
        remotePort: 57110,
        metadata: true
    });
}

var sc3 = defaultSc3Udp();
sc3.open();

/*
sc3.send(d_recv(d));
sc3.send(s_new0('g', -1, 1, 1));
sc3.send(g_freeAll1(1));
*/
