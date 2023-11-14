(* Select ; array expansion at first input *)
var m = [0 2 3 5 7 9] + 48;
var k = m.size + 1;
var x = MouseX(0, k, 0, 0.2);
var y = MouseY(0, k, 0, 0.2);
SinOsc(Select([x, y], m).MidiCps, 0) * 0.1
