(* MouseX ; k random sine tones across stereo field, MouseX selects which to listen to ; https://scsynth.org/t/5722/4 *)
var k = 20;
var mouseX = MouseX(0, k, 0, 0.2);
var select = (0 .. k).collect { :i | mouseX > i };
var note = { [0, 2, 3, 5, 7, 9, 10].atRandom + [48, 60].atRandom } ! k;
var osc = SinOsc(note.MidiCps, 0) * select * 0.05;
osc.Splay

(* MouseX ; as above with reset ; rate is slower the more tones are audible *)
var k = 20;
var mouseX = MouseX(0, k, 0, 0.2);
var reset = LfNoise2(k * 2 / (mouseX + 1)).kr;
var select = (0 .. k).collect { :i | mouseX > i };
var note = { Choose(reset, [0 2 3 5 7 9 10]) + Choose(reset, [48 60]) } ! k;
var osc = SinOsc(note.MidiCps, 0) * select * 0.05;
osc.Splay

(* Mouse control of Impulse frequency *)
Impulse([4 5 9] * MouseX(0.01, 150, 1, 0.2), 0).Splay2 * 0.2
