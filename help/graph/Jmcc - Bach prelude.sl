;; jmcc ;  https://soundcloud.com/soundaspureform/bach-prelude
var p1 = [
	[60,64,67,72,76],[60,62,69,74,77],[59,62,67,74,77],[60,64,67,72,76],
	[60,64,69,76,81],[60,62,66,69,74],[59,62,67,74,79],[59,60,64,67,72],
	[57,60,64,67,72],[50,57,62,66,72],[55,59,62,67,71],[55,58,64,67,73],
	[53,57,62,69,74],[53,56,62,65,71],[52,55,60,67,72],[52,53,57,60,65],
	[50,53,57,60,65],[43,50,55,59,65],[48,52,55,60,64],[48,55,58,60,64],
	[41,53,57,60,64],[42,48,57,60,63],[44,53,59,60,62],[43,53,55,59,62],
	[43,52,55,60,64],[43,50,55,60,65],[43,50,55,59,65],[43,51,57,60,66],
	[43,52,55,60,67],[43,50,55,60,65],[43,50,55,59,65],[36,48,55,58,64]
];
var p2 = [
	[36,48,53,57,60,65,60,57,60,57,53,57,53,50,53,50],
	[36,47,67,71,74,77,74,71,74,71,67,71,62,65,64,62],
	{ 60 } ! 16
];
var p = p1.collect({ :x |
	var y = x ++ (3 .. 5).collect { :i | x[i] };
	[y, y].concatenation
}).concatenation ++ p2.concatenation;
var tr = Impulse(5, 0);
var freq = Demand(tr, 0, Dseq(1, p.MidiCps));
Pan2(FreeVerb(Lpf(Saw(Lag(freq, 0.03)), 1000), 0.3, 0.5, 0.35), 0, 0.2)
