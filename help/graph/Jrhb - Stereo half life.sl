(* stereo half-life (jrhb) *)
var tHalf = 3.92; (* Radon-219, discovered 1904 by Giesel and Debierne *)
var nAtoms = 100000; (* ca. 3.6e-14 mg *)
var n = (nAtoms - PulseCount(LocalIn(2, 0), 0)).Max(0);
var activity = Dust(n * 2.Log / tHalf);
(activity * 0.1) <! LocalOut(activity)
