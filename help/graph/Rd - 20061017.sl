(* 20061017 ; rd *)
var cf = [
	35.45 128.59 346.97 483.55 1049.24 1564.02 1756.33
	3391.66 3451.80 3497.26 3596.89 3696.73 3835.23 3845.95
	4254.85 4407.53 4415.26 4552.86 5538.07 5637.73 5690.29
	5728.00 5764.27 5824.41 6377.60 6544.35 6807.14 6994.97
	7026.84 7144.58 7269.61 7393.67 7897.25 8040.45 8157.77
	8225.01 9126.15 9488.52 9916.40 10155.59 11715.95
	12111.83 12339.99 12417.66 12459.28 12618.33 13116.49
	13201.12 13297.83 13533.75
];
var ca = [
	0.001282 0.000804 0.017361 0.004835 0.004413 0.004110
	0.000333 0.003614 0.006919 0.000322 0.000603 0.066864 0.000605
	0.003602 0.000283 0.015243 0.020536 0.016677 0.000924 0.202050
	0.001254 0.012705 0.000252 0.000486 0.000642 0.000776 0.208116
	0.002491 0.001934 0.005231 0.006924 0.001203 0.205002 0.040604
	0.003834 0.002189 0.180560 0.002192 0.006516 0.009982 0.004745
	0.046154 0.000510 0.001890 0.001978 0.006729 0.002342 0.002400
	0.035155 0.001408
];
var cd = [
	5.20368 1.70343 40.16516 27.28250 0.89505 42.84742
	2.66036 15.76788 6.84836 3.23250 1.73433 2.02024
	4.72790 9.40010 0.71025 37.49462 36.24879 29.17265
	3.89101 4.75788 3.85142 20.90781 3.73287 2.38341
	10.44328 8.79561 20.98564 18.01180 25.29788 14.81981
	42.39189 2.94851 11.04376 49.55165 29.88269 10.52718
	23.55724 26.55561 45.09960 22.55039 36.46126 11.82620
	16.81818 14.90312 32.81113 43.13890 12.28955 11.49894
	10.46578 24.93169
];
var ps = [-12 -5 0 2 4 5 7 12];
{
	var tr = Dust(1 / 3);
	var fs = Select(IRand(tr, 0, 7), ps);
	var k = Ringz(
		Decay2(tr, 0.06, 0.01) * PinkNoise() * Rand(tr, 0, 1),
		cf * fs.MidiRatio + Rand(tr, 0, 1),
		1 / cd * Rand(tr, 2, 17)
	) * ca;
	EqPan2(k.Sum, Rand(tr, -1, 1))
} !> 7
