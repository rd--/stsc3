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

// Operators

function UnaryOp(ix, a) {
    return makeUgen('UnaryOpUGen', 1, inputRate([a]), ix, [a]);
}

function BinaryOp(ix, a, b) {
    return makeUgen('BinaryOpUGen', 1, inputRate([a, b]), ix, [a, b]);
}

// Operator bindings

function Neg(a) { return UnaryOp(0, a); }
function Not(a) { return UnaryOp(1, a); }
function IsNil(a) { return UnaryOp(2, a); }
function NotNil(a) { return UnaryOp(3, a); }
function BitNot(a) { return UnaryOp(4, a); }
function Abs(a) { return UnaryOp(5, a); }
function AsFloat(a) { return UnaryOp(6, a); }
function AsInt(a) { return UnaryOp(7, a); }
function Ceil(a) { return UnaryOp(8, a); }
function Floor(a) { return UnaryOp(9, a); }
function Frac(a) { return UnaryOp(10, a); }
function Sign(a) { return UnaryOp(11, a); }
function Squared(a) { return UnaryOp(12, a); }
function Cubed(a) { return UnaryOp(13, a); }
function Sqrt(a) { return UnaryOp(14, a); }
function Exp(a) { return UnaryOp(15, a); }
function Recip(a) { return UnaryOp(16, a); }
function MidiCps(a) { return UnaryOp(17, a); }
function CpsMidi(a) { return UnaryOp(18, a); }
function MidiRatio(a) { return UnaryOp(19, a); }
function RatioMidi(a) { return UnaryOp(20, a); }
function DbAmp(a) { return UnaryOp(21, a); }
function AmpDb(a) { return UnaryOp(22, a); }
function OctCps(a) { return UnaryOp(23, a); }
function CpsOct(a) { return UnaryOp(24, a); }
function Log(a) { return UnaryOp(25, a); }
function Log2(a) { return UnaryOp(26, a); }
function Log10(a) { return UnaryOp(27, a); }
function Sin(a) { return UnaryOp(28, a); }
function Cos(a) { return UnaryOp(29, a); }
function Tan(a) { return UnaryOp(30, a); }
function ArcSin(a) { return UnaryOp(31, a); }
function ArcCos(a) { return UnaryOp(32, a); }
function ArcTan(a) { return UnaryOp(33, a); }
function Sinh(a) { return UnaryOp(34, a); }
function Cosh(a) { return UnaryOp(35, a); }
function Tanh(a) { return UnaryOp(36, a); }
function Rand_(a) { return UnaryOp(37, a); }
function Rand2(a) { return UnaryOp(38, a); }
function LinRand_(a) { return UnaryOp(39, a); }
function BiLinRand(a) { return UnaryOp(40, a); }
function Sum3Rand(a) { return UnaryOp(41, a); }
function Distort(a) { return UnaryOp(42, a); }
function SoftClip(a) { return UnaryOp(43, a); }
function Coin(a) { return UnaryOp(44, a); }
function DigitValue(a) { return UnaryOp(45, a); }
function Silence(a) { return UnaryOp(46, a); }
function Thru(a) { return UnaryOp(47, a); }
function RectWindow(a) { return UnaryOp(48, a); }
function HanWindow(a) { return UnaryOp(49, a); }
function WelchWindow(a) { return UnaryOp(50, a); }
function TriWindow(a) { return UnaryOp(51, a); }
function Ramp_(a) { return UnaryOp(52, a); }
function Scurve(a) { return UnaryOp(53, a); }

function Add(a, b) { return BinaryOp(0, a, b); }
function Sub(a, b) { return BinaryOp(1, a, b); }
function Mul(a, b) { return BinaryOp(2, a, b); }
function Idiv(a, b) { return BinaryOp(3, a, b); }
function Fdiv(a, b) { return BinaryOp(4, a, b); }
function Mod(a, b) { return BinaryOp(5, a, b); }
function Eq(a, b) { return BinaryOp(6, a, b); }
function Ne(a, b) { return BinaryOp(7, a, b); }
function Lt(a, b) { return BinaryOp(8, a, b); }
function Gt(a, b) { return BinaryOp(9, a, b); }
function Le(a, b) { return BinaryOp(10, a, b); }
function Ge(a, b) { return BinaryOp(11, a, b); }
function Min(a, b) { return BinaryOp(12, a, b); }
function Max(a, b) { return BinaryOp(13, a, b); }
function BitAnd(a, b) { return BinaryOp(14, a, b); }
function BitOr(a, b) { return BinaryOp(15, a, b); }
function BitXor(a, b) { return BinaryOp(16, a, b); }
function Lcm(a, b) { return BinaryOp(17, a, b); }
function Gcd(a, b) { return BinaryOp(18, a, b); }
function Round(a, b) { return BinaryOp(19, a, b); }
function RoundUp(a, b) { return BinaryOp(20, a, b); }
function Trunc(a, b) { return BinaryOp(21, a, b); }
function Atan2(a, b) { return BinaryOp(22, a, b); }
function Hypot(a, b) { return BinaryOp(23, a, b); }
function Hypotx(a, b) { return BinaryOp(24, a, b); }
function Pow(a, b) { return BinaryOp(25, a, b); }
function ShiftLeft(a, b) { return BinaryOp(26, a, b); }
function ShiftRight(a, b) { return BinaryOp(27, a, b); }
function UnsignedShift(a, b) { return BinaryOp(28, a, b); }
function Fill(a, b) { return BinaryOp(29, a, b); }
function Ring1(a, b) { return BinaryOp(30, a, b); }
function Ring2(a, b) { return BinaryOp(31, a, b); }
function Ring3(a, b) { return BinaryOp(32, a, b); }
function Ring4(a, b) { return BinaryOp(33, a, b); }
function DifSqr(a, b) { return BinaryOp(34, a, b); }
function SumSqr(a, b) { return BinaryOp(35, a, b); }
function SqrSum(a, b) { return BinaryOp(36, a, b); }
function SqrDif(a, b) { return BinaryOp(37, a, b); }
function AbsDif(a, b) { return BinaryOp(38, a, b); }
function Thresh(a, b) { return BinaryOp(39, a, b); }
function AmClip(a, b) { return BinaryOp(40, a, b); }
function ScaleNeg(a, b) { return BinaryOp(41, a, b); }
function Clip2(a, b) { return BinaryOp(42, a, b); }
function Excess(a, b) { return BinaryOp(43, a, b); }
function Fold2(a, b) { return BinaryOp(44, a, b); }
function Wrap2(a, b) { return BinaryOp(45, a, b); }
function FirstArg(a, b) { return BinaryOp(46, a, b); }
function RandRange(a, b) { return BinaryOp(47, a, b); }
function ExpRandRange(a, b) { return BinaryOp(48, a, b); }

// Ugen bindings

// Interpolating sine wavetable oscillator.
function SinOsc(freq, phase) {
    return makeUgen('SinOsc', 1, Rate.ar, 0, [freq, phase]);
}

// 2nd order Butterworth highpass filter.
function HPF(input, freq) {
    return makeUgen('HPF', 1, inputRate([input]), 0, [input, freq]);
}

// Two channel equal power pan.
function Pan2(input, pos, level) {
    return makeUgen('Pan2', 2, inputRate([input]), 0, [input, pos, level]);
}

// Pink Noise.
function PinkNoise() {
    return makeUgen('PinkNoise', 1, Rate.ar, 0, []);
}

// Write a signal to a bus.
function Out(bus, channelsArray) {
    return makeUgen('Out', 0, inputRate([channelsArray]), 0, [bus].concat(inputAsArray(channelsArray)));
}

// Sawtooth oscillator
function LFSaw(freq, iphase) {
    return makeUgen('LFSaw', 1, Rate.ar, 0, [freq, iphase]);
}

// Comb delay line with no interpolation.
function CombN(input, maxdelaytime, delaytime, decaytime) {
    return makeUgen('CombN', 1, inputRate([input]), 0, [input, maxdelaytime, delaytime, decaytime]);
}

// Multiply add
function MulAdd(input, mul, add) {
    return makeUgen('MulAdd', 1, inputRate([input, mul, add]), 0, [input, mul, add]);
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

u = Mul(CombN(Mul(SinOsc(MidiCps(MulAdd(LFSaw(0.4, 0), 24, MulAdd(LFSaw([8, 7.23], 0), 3, 80))), 0), 0.04), 0.2, 0.2, 4), 0.1)

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
