;; https://scsynth.org/t//6353 ; io ; https://ingoogni.nl/
var shuheiKawachi = { :x :y :a :b |
	(x.cos * y.cos)
	+ (((a.sqrt * x - y) / b).cos * ((x + (a.sqrt * y) ) / b).cos)
	+ (((a.sqrt * x + y) / b).cos * ((x - (a.sqrt * y * y)) / b).cos);
};
var segments = 45; (* 50 ; udp *)
var xMajor = 0;
var yMajor = 10;
var rMajor = 30.1;
var rMinor = 0.1;
var circlefreq = 0.0001;
var xMinor = xMajor + (rMajor * SinOsc(circlefreq, 0));
var yMinor = yMajor + (rMajor * SinOsc(circlefreq, pi / 2));
var freqArray = { 50.ExpRand(1500) }.dup(segments).sorted;
var oscArray = (1 .. segments).collect { :index |
	var x = xMinor + (rMinor * (index * 2 * pi / segments).sin);
	var y = yMinor + (rMinor * (index * 2 * pi / segments).cos);
	var grey = shuheiKawachi(x, y, 2 * pi, 0.5);
	var amp = Lag(Wrap(grey, 0, 1), 0.01) / segments;
	SinOsc(freqArray[index], 0) * amp
};
Splay2(oscArray) * 5