(* Staffan Liljegren 980525 ; rd (edit) *)
{ :currentTime |
	(1 / 7).coin.ifFalse {
		var chord = [60 63 67; 65 68 72; 55 58 62].atRandom;
		var iot = (1 / [3 5 7 11 13 17]).atRandom;
		(iot * (0 .. 2)).do { :startTime |
			{
				var note = (chord.atRandom - [0 12 24].atRandom).MidiCps;
				var x = Decay(
					Impulse(0, 0) * Rand(0.1, 0.2),
					Rand(0.1 + 0.2)
				) * BrownNoise();
				var y = CombC(x, 0.05, 1 / note, Rand(2, 3));
				Release(EqPan(y, Rand(-1, 1)), 0, 3, 3)
			}.playAt(currentTime + startTime)
		}
	};
	1
}.schedule
