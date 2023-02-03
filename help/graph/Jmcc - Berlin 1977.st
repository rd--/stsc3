"berlin 1977 (jmcc) #4"
| sequ sequR clock_rate clock_time clock patternList note1 clock_16 note2 freq env amp filt pw sig |
sequ := [:s :tr | DmdOn trig: tr reset: 0 demandUGens: (Lseq repeats: Float infinity list: s)]. "somewhat akin to SC2 Sequencer"
sequR := [:s :tr | DmdOn trig: tr reset: 0 demandUGens: (Lshuf repeats: Float infinity list: s)]. "sequ with random selection function"
clock_rate := MouseX minval: 5 maxval: 20 warp: 1 lag: 0.2. "mouse x controls clock rate"
clock_time := clock_rate reciprocal.
clock := Impulse freq: clock_rate phase: 0. "sequencer trigger"
patternList := #(55 60 63 62 60 67 63 58).
note1 := sequ value: patternList value: clock. "midi note pattern sequencer"
clock_16 := PulseDivider trig: clock div: 16 start: 0. "divide clock by 16"
note2 := (sequR value: #(-12 -7 -5 0 2 5) value: clock_16) + note1. "transpose somewhat randomly"
freq := note2 midiCps. "convert midi note to cycles per second"
env := Decay2 in: clock attackTime: (clock_time * 0.05) decayTime: (clock_time * 2).
amp := env * 0.1 + 0.02. "amplitude envelope"
filt := env * (FSinOsc freq: 0.17 iphase: 0) * 800 + 1400. "filter frequency"
pw := SinOsc freq: 0.08 phase: {0. 0.5 * Float pi} mul: 0.45 add: 0.5. "pulse width Lfo(s)"
sig := Pulse freq: freq kr width: pw kr mul: amp kr.
CombN in: (Rlpf in: sig freq: filt kr rq: 0.15) maxdelaytime: 0.2 delaytime: #(0.2 0.17) decaytime: 1.5
