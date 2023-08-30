(* analog bubbles with mouse control (jmcc) #3 *)
var o1 = LfSaw(MouseX(2, 40, 1, 0.2), 0) * -3 + 80;
var o2 = LfSaw(MouseY(0.1, 10, 1, 0.2), 0) * 24 + o1;
CombN(SinOsc(o2.MidiCps, 0) * 0.04, 0.2, 0.2, 2)
