(* Release ; fixed duration envelope that releases (deletes) the sound on completion *)
Release(SinOsc(440, 0) * 0.1, 0.01, 3, 7)

(* ---- Release ; keywords *)
Release(
	in: SinOsc(440, 0) * 0.1,
	attackTime: 0.01,
	sustainTime: 3,
	releaseTime: 7
)

(* ---- Release ; process ; requires=fork *)
9.timesRepeat {
	{ Release(Pan2(SinOsc(Rand(200, 400), 0), Rand(-1, 1), Rand(0.01, 0.1)), 0.01, 3, 7) }.play;
	3.seconds.wait
}
