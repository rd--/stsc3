(* SplayAz ; Stereo ; 2->2 *)
var osc = SinOsc([333, 555], 0) * 0.1;
var orientation = MouseX(0, 1, 0, 0.2);
SplayAz(2, osc, 1, 1, 2, 0, orientation, true)

(* SplayAz ; Octophonic ; 2->8 ; Formerly error, also at Sc *)
var osc = SinOsc([333, 555], 0) * 0.1;
var orientation = MouseX(0, 1, 0, 0.2);
SplayAz(8, osc, 1, 1, 2, 0, orientation, true)

(* SplayAz ; Octophonic ; 24->8 *)
var numChannels = 8;
var numVoices = 24;
var orientation = 0;
var inArray = (1 .. numVoices).collect { :each |
	SinOsc(LfNoise2(Rand(10, 20)) * 200 + (each * 50 + 400), 0) * 0.1
};
SplayAz(8, inArray, 1, 1, 2, 0, orientation, true)

(* SplayAz ; mouse control *)
var numChannels = 8;
var numVoices = 16;
var inArray = (1 .. numVoices).collect { :each |
	SinOsc(LfNoise2(Rand(10, 20)) * 200 + (each * 100 + 400), 0)
};
var spread = MouseY(1, 0, 0, 0.2);
var level = 0.2;
var width = 2;
var center = MouseX(-1, 1, 0, 0.2);
var orientation = 0.5;
var levelComp = true;
SplayAz(
	numChannels,
	inArray,
	spread,
	level,
	width,
	center,
	orientation,
	levelComp
)
