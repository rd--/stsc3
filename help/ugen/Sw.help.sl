(* ---- Sw ; switch ; turn oscillator on and off ; requires=Sw *)
(1 .. 8).collect { :i | SinOsc(110 * i, 0) * Sw(i) * 0.1 }.Splay2

(* Sw ; switch ; turn oscillator (with amplitude lfo) on and off *)
(1 .. 8).collect { :i | SinOsc(110 * i, 0) * Sw(i) * SinOsc(0.05, i * pi / 4) * 0.1 }.Splay2

(* Sw ; switch ; switch is gate *)
(1 .. 8).collect { :i | SinOsc(110 * i, 0) * Asr(Sw(i), 0.01, 1, -4) * 0.1 }.Splay2

(* Sw ; switch ; switch is trigger *)
(1 .. 8).collect { :i | SinOsc(110 * i, 0) * Decay(Trig(Sw(i), 0.001), 1) * 0.1 }.Splay2
