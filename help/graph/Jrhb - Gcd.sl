(* generative landscape (jrhb) ; https://www.listarc.bham.ac.uk/lists/sc-users/msg68991.html *)
var x = MouseX(-10, 10, 0, 0.1);
var y = MouseY(0, 5, 0, 0.1);
var i = (LfSaw(0.062, 0) * 10) + (y * [0 .. 8]);
var d = i.Frac;
var a = ModDif(d, 0.5, 1).Neg + 0.8;
var z1 = i.Gcd(x) + d;
var z2 = i.Lcm(x) + d;
var freq = [z1, z2].transposed.Abs + 1 * 120;
var c = AmpComp(freq, 261.625, 1/3) * a;
(SinOsc(freq, 0) * c).Mix * 0.01
