(* TScramble ; octaves, swaps channels on impulse, scramble may be identity *)
var tr = Impulse(1, 0);
SinOsc(TScramble(tr, [220, 440]), 0) * 0.1

(* TScramble ; c-major sin tones across stereo field, scrambles on impulse *)
var tr = Impulse(1, 0);
SinOsc(TScramble(tr, [60 62 64 65 67 69 71 72].MidiCps), 0).Splay * 0.1

(* TScramble ; initially unscrambled *)
var tr = Dust(1);
SinOsc(TScramble(tr, [60 62 64 65 67 69 71 72].MidiCps), 0).Splay * 0.1

(* TScramble ; c-major sin tones across stereo field, scrambles on impulse *)
var tr = Impulse(1, 0);
SinOsc(TScramble(tr, [60, 62, 64, 65, 67, 69, 71, 72].MidiCps), 0).Splay * 0.1

(* TScramble ; sequences of different lengths, scrambled *)
var a = [[1, 2, 3], [1, 2, 3, 4, 5], [1, 2, 3, 4, 5, 6, 7]];
var t1 = Impulse(1 / 5, 0);
var t2 = Impulse(5, 0);
var m = Choose(t1, a.collect { :x | Demand(t2, 0, Dseq(inf, TScramble(t1, x))) });
var c = SinOsc(1200, 0) * Decay(t1, 1) * 0.1;
SinOsc(m * 110, 0) * 0.1 + c

(* TScramble ; reference *)
SinOsc([60 62 64 65 67 69 71 72].MidiCps, 0).Splay * 0.1
