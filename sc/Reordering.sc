Seq { *new { arg repeats, list; ^Dseq.multiNewList(['demand', repeats] ++ list) } }
Shuf { *new { arg repeats, list; ^Dshuf.multiNewList(['demand', repeats] ++ list) } }
Choose { *new { arg repeats, list; ^Drand.multiNewList(['demand', repeats] ++ list) } }
DmdOn { *new { arg trig, reset, demandUGens; ^Demand.multiNewList([trig.rate, trig, reset] ++ demandUGens.asArray) } }
DmdFor { *new { arg dur, reset, level; ^Duty.multiNew('audio', dur, reset, 0, level) } }
BufRec { *new { arg bufnum, reset, inputArray; ^RecordBuf.ar(inputArray, bufnum, 0, 1, 0, 1, 1, reset, 0) } }
Ln { *new { arg start = 0.0, end = 1.0, dur = 1.0; ^Line.performList('audio'.rateToSelector, [start, end, dur]) } }
XLn { *new { arg start = 0.0, end = 1.0, dur = 1.0; ^XLine.performList('audio'.rateToSelector, [start, end, dur]) } }