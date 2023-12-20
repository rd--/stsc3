"https://sccode.org/1-590 (dm) ; this prints infinity somewhere (divide by zero)"
| shiftRegister amp pulse rate trans trig octave note chord cf mf sig cmp rvb |
shiftRegister := [ :length :tr :in |
	| buf count rd wr |
	buf := BufAlloc numChannels: 1 numFrames: length.
	count := PulseCount trig: tr reset: 0.
	rd := DmdOn trig: tr reset: 0 demandUGens: (Dbufrd bufnum: buf phase: count + length iota loop: 1) reversed.
	wr := DmdOn trig: tr reset: 0 demandUGens: (Dbufwr bufnum: buf phase: count input: in loop: 1).
	rd <% wr
].
amp := 0.1.
pulse := Impulse freq: 1 / 16 phase: 0.
rate := TChoose trig: pulse array: #(3 5 10).
trans := TChoose trig: pulse array: #(0 2 -2 7 -5).
trig := Trig1 in: (CuspL freq: rate kr * 3 a: 1 b: 1.9 xi: 0) dur: 0.001.
octave := DmdOn trig: (PulseDivider trig: trig div: 4 start: 0) reset: 0 demandUGens: (Drand repeats: Float infinity list: #(12 -12)).
note := DmdOn trig: trig reset: 0 demandUGens: (Dseq repeats: Float infinity list: #(42 46 51 54 59 63 66) shuffled + trans kr + octave).
chord := shiftRegister value: 5 value: trig value: note midiCps.
cf := Vibrato freq: chord rate: 6 depth: 0.02 delay: 0 onset: 0 rateVariation: 0.04 depthVariation: 0.1 iphase: 0 trig: 0.
mf := chord * (LinLin in: (LfPulse freq: 1 / 8 iphase: 0 width: 0.5) srclo: -1 srchi: 1 dstlo: 1.01 dsthi: 2.01).
sig := PmOsc carfreq: cf modfreq: mf pmindex: ((EnvSpec levels: #(3 3 0) times: #(0 0.2) curves: #(-4 -4)) asEnvGen: trig) modphase: 0.
cmp := (sig * (AmpCompA freq: chord root: 0 minAmp: 0.32 rootAmp: 1) * amp) sum.
rvb := GVerb in: (Bpf in: cmp freq: 90 midiCps rq: 1) roomsize: 50 revtime: 8 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 0 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300.
XFade2 inA: {cmp. cmp} inB: rvb pan: 0.2 level: 1
