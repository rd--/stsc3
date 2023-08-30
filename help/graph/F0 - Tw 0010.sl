(* https://sccode.org/1-4Qy ; f0 ; 0010 *)
LeakDc(
	SinOsc(
		SinOsc(0.31, 0),
		SinOsc(
			SinOsc(0.21, 0),
			SinOsc(
				SinOsc(0.11, SinOsc(0.01, 0)),
				0
			) * SinOsc([2, 3], 0) * 400
		)
	) * SinOsc([0.3, 0.21], 0),
	0.995
)
