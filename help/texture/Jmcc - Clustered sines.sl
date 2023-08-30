(* clustered sines (jmcc) #2 *)
{
	var n = 80;
	var f1 = Rand(100, 1100);
	var f2 = 4.0 * f1;
	{
		var y = { f1 + f2.Rand } ! n;
		var a = f1 / y;
		SinOscBank(y, a, nil) / n
	} ! 2
}.xfade(4, 4)
