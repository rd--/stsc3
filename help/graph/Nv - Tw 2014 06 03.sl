(* https://twitter.com/headcube/status/474064500564324352 (nv) *)
var k = 9;
{
	var f = (1 .. 8).collect{ :ix |
		{
			LfPulse(2 ^ IRand(-9, 1), IRand(0, 2) / 2, 0.5)
		} !* (ix + 1) / ix + 1
	}.product * 86;
	Pluck(Bpf(f, f, 1).Sin, Saw(440), 1 , 1 / f, 9, 0.5)
} !^ k * 0.2
