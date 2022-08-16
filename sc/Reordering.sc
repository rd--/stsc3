Seq { *new { arg repeats, list; ^Dseq.dr(list, repeats) } }
Ser { *new { arg repeats, list; ^Dser.dr(list, repeats) } }
Series { *new { arg length, start, step; ^Dseries.dr(start, step, length) } }
Shuf { *new { arg repeats, list; ^Dshuf.dr(list, repeats) } }
Choose { *new { arg repeats, list; ^Drand.dr(list, repeats) } }
Xchoose { *new { arg repeats, list; ^Dxrand.dr(list, repeats) } }
White { *new { arg length = inf, lo = 0, hi = 1; ^Dwhite.dr(lo, hi, length) } }

DmdOn { *new { arg trig, reset, demandUGens; ^Demand.multiNewList([trig.rate, trig, reset] ++ demandUGens.asArray) } }
DmdFor { *new { arg dur=1, reset=0, level=1; ^Duty.ar(dur, reset, level, 0) } }
TDmdFor { *new { arg dur=1, reset=0, level=1; ^TDuty.ar(dur, reset, level, 0, 0) } }

BufRec { *new { arg bufnum, reset, inputArray; ^RecordBuf.ar(inputArray, bufnum, 0, 1, 0, 1, 1, reset, 0) } }
BufAlloc { *new { arg numChannels, numFrames; ^LocalBuf.ir(numFrames, numChannels) } } // .clearBuf?
BufWrite { *new { arg bufnum, phase, loop, inputArray; ^BufWr.ar(inputArray, bufnum, phase, loop) } }

InFb { *new { arg numChannels, bus; ^InFeedback.ar(bus, numChannels) } }
