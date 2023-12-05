(* one line (ljp) *)
var lfs = (LfSaw([1, 0.99], [0, 0.6]) * 2000 + 2000);
var sft = lfs.Trunc([400, 600]) * [1, -1];
EqPan2(SinOsc(OnePole(sft.Sum, 0.98), 0), 0) / 10
