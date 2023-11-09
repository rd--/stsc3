(* SplayAz *)
var numChannels = 8;
var numVoices = 24;
var inArray = (1 .. numVoices).collect { :each |
	SinOsc(LfNoise2(Rand(10, 20)) * 200 + (each * 50 + 400), 0) * 0.1
};
Silent(8) ++ SplayAz(8, inArray, 1, 1, 2, 0, 0, true)

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
Silent(8) ++ SplayAz(
	numChannels,
	inArray,
	spread,
	level,
	width,
	center,
	orientation,
	levelComp
)
