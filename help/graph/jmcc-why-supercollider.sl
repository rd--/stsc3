;; why supercollider (jmcc) #0
var s = { Resonz(Dust(0.2) * 50,  200 + 3000.Rand, 0.003) }.dup(10).sum;
var x = { CombL(DelayN(s, 0.048, 0.048), 0.1, LfNoise1(0.1.Rand) * 0.04 + 0.05, 15) }.dup(7).sum;
4.timesRepeat { x := AllpassN(x, 0.050, { 0.050.Rand }.dup, 1) };
s + (x * 0.2)

;; why supercollider (jmcc) #0 ; keywords
var s = {
	Resonz(
		in: Dust(density: 0.2) * 50,
		freq: Rand(lo: 200, hi: 3200),
		bwr: 0.003
	)
}.dup(10).sum;
var z = DelayN(
	in: s,
	maxdelaytime: 0.048,
	delaytime: 0.048
);
var x = {
	CombL(
		in: z,
		maxdelaytime: 0.1,
		delaytime: LfNoise1(freq: Rand(lo: 0, hi: 0.1)) * 0.04 + 0.05,
		decaytime: 15
	)
}.dup(7).sum;
4.timesRepeat {
	x := AllpassN(
		in: x,
		maxdelaytime: 0.050,
		delaytime: [Rand(lo: 0, hi: 0.05), Rand(lo: 0, hi: 0.050)],
		decaytime: 1
	)
};
s + (x * 0.2)
