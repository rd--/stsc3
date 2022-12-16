;; https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) L43 ; keyword parameters ; ?
var f = { :i |
	  var x = Impulse(freq: 1, phase: i / 10) + Impulse(freq: 0, phase: 0);
	  var o = LfSaw(freq: [102, 101], iphase: 0);
	  var d = 1 / Latch(in: 1.015 ** Sweep(trig: Impulse(freq: 0, phase: 0), rate: 1) * 64 % 1 + 1 * 200, trig: x);
	  Pluck(in: o, trig: x, maxdelaytime: 1, delaytime: d, decaytime: 4, coef: 0.2)
};
(0 .. 9).collect(f).mean * 0.1
