// combc ; event control
var lfo, osc; lfo = SinOsc(0.5, 0); osc = Voicer(16, function(e) {  return mul(mul(mul(SinOsc(unitCps(p(e)), 0), lfo), w(e)), z(e)); }); CombC(splay(osc), 0.5, 0.2, 3)