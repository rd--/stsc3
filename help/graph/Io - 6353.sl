(* https://scsynth.org/t/6353 ; io ; https://ingoogni.nl/ *)
var shuheiKawachi = { :x :y :a :b |
	[
		(x.Cos * y.Cos),
		(((a.Sqrt * x - y) / b).Cos * ((x + (a.Sqrt * y)) / b).Cos),
		(((a.Sqrt * x + y) / b).Cos * ((x - (a.Sqrt * y * y)) / b).Cos)
	].Sum
};
var segments = 50;
var xMajor = 0;
var yMajor = 10;
var rMajor = 30.1;
var rMinor = 0.1;
var circlefreq = 0.0001;
var xMinor = xMajor + (rMajor * SinOsc(circlefreq, 0));
var yMinor = yMajor + (rMajor * SinOsc(circlefreq, pi / 2));
var freqArray = { 50.ExpRand(1500) }.duplicate(segments).sorted;
var oscArray = (1 .. segments).collect { :index |
	var x = xMinor + (rMinor * (index * 2 * pi / segments).Sin);
	var y = yMinor + (rMinor * (index * 2 * pi / segments).Cos);
	var grey = shuheiKawachi(x, y, 2 * pi, 0.5);
	var amp = Lag(Wrap(grey, 0, 1), 0.01) / segments;
	SinOsc(freqArray[index], 0) * amp
};
oscArray.Splay * 5
