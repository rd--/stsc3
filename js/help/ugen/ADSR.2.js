// ADSR ; event control
var f; f = function(e) {  return mul(Blip(unitCps(p(e)), mul(o(e), 5)), ADSR(w(e), mul(0.5, y(e)), mul(y(e), 0.75), 0.65, mul(y(e), 4), -4)); }; mul(Splay2(Voicer(16, f)), 0.1)