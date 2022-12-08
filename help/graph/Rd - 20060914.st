" 20060914 ; rd ; graph rewrite"
| gen |
gen := [ :tr |
	| chrd scale octaves mnn chd |
	chrd := [ :m |
		| ds du d f e p o |
		ds := 3.
		du := #(5 4 5 7 4 5).
		d := du * ds.
		f := (TxLine start: m end: (TRand lo: 0.05 hi: 0.5 trig: tr) + m dur: d trig: tr) midiCps.
		e := (Sine trig: tr dur: du maxItem * ds) * (TRand lo: 0.005 hi: 0.01 trig: tr).
		p := TxLine start: (TRand lo: -1 hi: 1 trig: tr) end: (TRand lo: -1 hi: 1 trig: tr) dur: d trig: tr.
		o := SinOsc freq: f kr phase: 0.
		(Pan2 in: o * e pos: p level: 1) sum
	].
	scale := #(0 2 4 5 7 9 11).
	octaves := #(4 5 6 7).
	mnn := (scale collect: [ :n | octaves collect: [ :o | n + (o * 12) ] ]) concatenation.
	chd := [ TChoose trig: tr array: mnn ] dup: 6.
	([ chrd value: chd ] dup: 9) sum].
OverlapTexture graphFunc: gen sustainTime: 21 transitionTime: 0 overlap: 3
