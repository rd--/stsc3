// VoiceWriter ; write w and p fields of first 8 voices
VoiceWriter(8, function(e) { var w, p; w = Trig(Impulse(Rand(0.15, 0.85), Rand(0, 1)), 0.25); p = Latch(LinLin(SinOsc(Rand(0.25, 1.5), 0), -1, 1, 0.15, 0.95), tr); return fromAssocArray(e, [->('w', w), ->('p', p)]); })