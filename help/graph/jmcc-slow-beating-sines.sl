;; slow beating sines (jmcc) #7 ; texture=xfade,4,4,inf
var n = 20;
var d = 5;
var p = OrderedCollection.new;
var q = OrderedCollection.new;
var f = { :freq |
	SinOscBank(freq.asArray, [1], { Rand(0, 2 * pi) } ! (3 * n))
};
n.timesRepeat {
	var freq = Rand(24, 84).MidiCps;
	p.add(freq);
	{ p.add(freq + d.Rand2) } ! 2;
	{ q.add(freq + d.Rand2) } ! 3
};
[p, q].collect(f) * 0.1 / n
