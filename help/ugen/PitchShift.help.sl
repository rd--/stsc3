(* use PitchShift to granulate input ; upper left corner is normal playback ; x = pitch dispersion, y = time dispersion *)
var grainSize = 0.1;
var in = AudioIn([1, 2]) * 0.5;
var x = MouseX(0, 1, 0, 0.2);
var y = MouseY(0, grainSize, 0, 0.2);
PitchShift(in, grainSize, 1, x, y)
