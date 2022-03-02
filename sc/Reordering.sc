Seq { *new { arg repeats, list; ^Dseq.dr(list, repeats) } }
Ser { *new { arg length, start, step; ^Dseries.dr(start, step, length) } }
Shuf { *new { arg repeats, list; ^Dshuf.dr(list, repeats) } }
Choose { *new { arg repeats, list; ^Drand.dr(list, repeats) } }
DmdOn { *new { arg trig, reset, demandUGens; ^Demand.multiNewList([trig.rate, trig, reset] ++ demandUGens.asArray) } }
DmdFor { *new { arg dur, reset, level; ^Duty.ar(dur, reset, level, 0) } }
TDmdFor { *new { arg dur, reset, level; ^TDuty.ar(dur, reset, level, 0, 0) } }

BufRec { *new { arg bufnum, reset, inputArray; ^RecordBuf.ar(inputArray, bufnum, 0, 1, 0, 1, 1, reset, 0) } }
BufAlloc { *new { arg numChannels, numFrames; ^LocalBuf.ir(numFrames, numChannels) } } // .clearBuf?

DelayWrite { *new { arg bufnum, in; ^RecordBuf.ar([in], bufnum, 0, 1, 0, 1, 1, 1, 0) } }
DelayTap { *new { arg bufnum, delayTime; ^PlayBuf.ar(1, bufnum, 1, 1, BufDur.kr(bufnum) - delayTime * SampleRate.ir, 1, 0) } }

InFb { *new { arg numChannels, bus; ^InFeedback.ar(bus, numChannels) } }

Ln {
    *new { arg start, end, dur; ^Line.ar(start, end, dur, 1, 0, 0) }
    *ar { arg start, end, dur; ^Line.ar(start, end, dur, 1, 0, 0) }
    *kr { arg start, end, dur; ^Line.kr(start, end, dur, 1, 0, 0) }
}

XLn {
    *new { arg start = 0.0, end = 1.0, dur = 1.0; ^XLine.ar(start, end, dur, 1, 0, 0) }
    *kr { arg start = 0.0, end = 1.0, dur = 1.0; ^XLine.kr(start, end, dur, 1, 0, 0) }
}

Splay2 {
    *ar { arg inArray; ^Splay.ar(inArray, 1, 1, 0, true) }
    *new { arg inArray; ^Splay2.ar(inArray) }
}
