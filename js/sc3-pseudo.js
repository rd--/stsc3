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

function DmdFor(dur, reset, level) {
    return Duty(dur, reset, 0, level);
}

function DmdOn(trig, reset, demandUGens) {
    return Demand(trig, reset, demandUGens);
}

var Seq = Dseq;
var Ser = Dseries;
var Shuf = Dshuf;
var Choose = Drand;

function Ln(start, end, dur) {
        return Line(start, end, dur, 0);
}

function TLine(start, end, dur, trig) {
    var env = Env([start, start, end], [0, dur], 'lin', null, null, 0);
    return EnvGen(trig, 1, 0, 1, 0, env.coord());
}

function TXLine(start, end, dur, trig) {
    var env = Env([start, start, end], [0, dur], 'exp', null, null, 0);
    return EnvGen(trig, 1, 0, 1, 0, env.coord());
}

function bitShiftRight(a, b) {
    return shiftRight(a, b);
}

function AudioIn(channels) {
    return In(1, sub(add(NumOutputBuses(), channels), 1));
}

/*
note that mrg places q in p, and here q has a reference to p, so the traversal of the mrg node must not recurse

b = asLocalBuf([0, 2, 4, 5, 7, 9, 11]);
c = [];
ugenTraverseCollecting(b, c, [])
*/
function asLocalBuf(array) {
    var k = array.length;
    var p = LocalBuf(1, k);
    var q = SetBuf(p, 0, k, array);
    return mrg(p, q);
}

function clearBuf(buf) {
    return mrg(buf, ClearBuf(buf));
}

function BufRec(bufnum, reset, inputArray) {
    return RecordBuf(bufnum, 0, 1, 0, 1, 1, reset, 0, inputArray);
}

var BufAlloc = LocalBuf;

// Reshape input arrays, and allow amp and time to be null (defaulting to 1)
function asKlankSpec(freq, amp, time) {
    var n = freq.length;
    var a = [freq, amp || arrayReplicate(n, 1), time || arrayReplicate(n, 1)];
    // console.log('asKlankSpec', a);
    return a.extendToBeOfEqualSize().transpose().concatenation();
}

function RingzBank(input, freq, amp, time) {
    return Klank(input, 1, 0, 1, asKlankSpec(freq, amp, time));
}

function SinOscBank(freq, amp, time) {
    return Klang(1, 0, asKlankSpec(freq, amp, time));
}

function LinSeg(gate, coordArray) {
    var coord = coordArray.clump(2).transpose();
    var levels = first(coord);
    var times = second(coord);
    var env = Env(levels, times.slice(0, times.length - 1), 'lin', null, null, 0);
    return EnvGen(gate, 1, 0, 1, 0, env.coord());
}
