(* SparseMatrixMixer ; identity ; copy 2-channel input to 2-channel output *)
SparseMatrixMixer(2, SinOsc([220, 221], 0) * 0.1, [1 1 1; 2 2 1])

(* SparseMatrixMixer ; copy 2-channel input to channels 9 & 13 of 24-channel output *)
SparseMatrixMixer(24, SinOsc([220, 221], 0) * 0.1, [1 9 1; 2 13 1])

(* SparseMatrixMixer ; randomly copy from 4-channel input to 24-channel output *)
{
	SparseMatrixMixer(
		24,
		{ SinOsc(Rand(220, 550), 0) } ! 4,
		{ [(1 .. 4).atRandom, (1 .. 24).atRandom, Rand(0.05, 0.15)] } ! 6
	)
}.OverlapTexture(6, 5, 4)

(* SparseMatrixMixer ; displace 4-channel input by two places at 6-channel output *)
SparseMatrixMixer(6, SinOsc([32 .. 35], 0) * 0.25, [1 3 1; 2 4 1; 3 5 1; 4 6 1])
