;; slow beating sines (jmcc) #7 ; texture=xfade,4,4,inf
var n = 20;
var d = 5.0;
var p = OrderedCollection.new;
var q = OrderedCollection.new;
var f = { :freq |
	SinOscBank(freq.asArray, [1], { 0.0.rrand(2 * pi) } ! (3 * n))
};
n.timesRepeat {
	var freq = 24.0.rrand(84.0).midiCps;
	p.add(freq);
	p.add(freq + d.rand2);
	p.add(freq + d.rand2);
	q.add(freq + d.rand2);
	q.add(freq + d.rand2);
	q.add(freq + d.rand2)
};
[p, q].collect(f) * 0.1 / n