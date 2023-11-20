(* ---- SfPlay ; normal playback at same speed of recording ; loop ; requires=SfAcquire *)
var sf = SfAcquireMono('floating_1');
SfPlay(sf, 1, 1, 0, 1)

(* SfPlay ; normal playback at same speed of recording ; no loop, retrigger *)
var sf = SfAcquireMono('floating_1');
SfPlay(sf, 1, Impulse(1 / SfDur(sf), 0), 0, 0)

(* SfPlay ; mouse controls playback rate and re-trigger interval ; loop *)
var sf = SfAcquireMono('floating_1');
var rateMultiplier = MouseX(0.25, 4, 1, 0.2);
SfPlay(sf, rateMultiplier, 1, 0, 1)

(* SfPlay ; mouse controls playback rate and re-trigger interval ; no loop, retrigger *)
var sf = SfAcquireMono('floating_1');
var rateMultiplier = MouseX(0.25, 4, 1, 0.2);
SfPlay(sf, rateMultiplier, Impulse(rateMultiplier / SfDur(sf), 0), 0, 0)

(* SfPlay ; recursive scrubbing (adc) ; https://www.listarc.cal.bham.ac.uk/lists/sc-users-2002/msg00736.html *)
var sf = SfAcquireMono('floating_1');
var r = 10;
(1 .. 6).do { :i |
	r := LfNoise1(0.2 ^ i * 50) * r
};
SfPlay(sf, r, 1, [0, 18000], 1)

(* SfPlay ; floating dust (adc) ; https://www.listarc.cal.bham.ac.uk/lists/sc-users-2002/msg00736.html ; panning edit (rd) *)
var sf = SfAcquireMono('floating_1');
(0 .. 9).collect { :n |
	var r = 0.1 ^ (n - 1);
	(0 .. n).collect { :i |
		var a = (n - 2 < i).if {
			0.5
		} {
			0.1 ^ (n * 0.9 - 3.5 - i)
		};
		r := SfPlay(sf, r, 1, 0, 1) * a
	};
	EqPan2(r, Rand(-1, 1))
}.Mix

(* SfPlay ; 2 min (adc) ; https://www.listarc.cal.bham.ac.uk/lists/sc-users-2002/msg00736.html *)
var sf = SfAcquireMono('floating_1');
var l = XLine(1, 0.007, 120);
SfPlay(sf, Decay(Dust(1 / (l * l)) * l.Sqrt, 1), 1, l, 1)
