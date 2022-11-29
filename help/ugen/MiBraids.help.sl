;; MiBraids ; basic example
MiBraids(pitch: 60, timbre: 0.5, color: 0.5, model: 6, trig: 0, resamp: 0, decim: 0, bits: 0, ws: 0) ! 2 * 0.05

;; MiBraids ; some modulation
var mod = LfNoise1(0.5) * 0.5 + 0.5;
MiBraids(pitch: 40, timbre: mod, color: 0, model: 1, trig: 0, resamp: 0, decim: 0, bits: 0, ws: 0) ! 2 * 0.05

;; MiBraids ; vosim
var pitch = LinLin(LfNoise0(4), -1, 1, 33, 66).rounded;
var timbre = LfNoise1(0.3) * 0.5 + 0.5;
var color = LfNoise1(0.3) * 0.5 + 0.5;
MiBraids(pitch: pitch, timbre: timbre, color: color, model: 21, trig: 0, resamp: 0, decim: 0, bits: 0, ws: 0) ! 2 * 0.1

;; MiBraids ; fluted
var pitch = 38;
var timbre = MouseX(0.7, 1, 0, 0.2);
var color = MouseY(0, 1, 0, 0.2);
MiBraids(pitch: pitch, timbre: timbre, color: color, model: 31, trig: 0, resamp: 1, decim: 0, bits: 0, ws: 0) ! 2 * 0.1

;; MiBraids ; scanning
var pitch = MouseY(33, 72, 0, 0.2);
var timbre = LfNoise1(0.3) * 0.5 + 0.5;
var color = LfNoise1(0.3) * 0.5 + 0.5;
var model = MouseX(0, 47, 0, 0.2);
MiBraids(pitch: pitch, timbre: timbre, color: color, model: model, trig: 0, resamp: 1, decim: 0, bits: 0, ws: 0) ! 2 * 0.1

;; MiBraids ; paraphonic
var timbre = LfNoise1(0.03) * 0.5 + 0.5;
var color = LfNoise1(0.05) * 0.5 + 0.5; (* chord *)
MiBraids(pitch: 38, timbre: timbre, color: color, model: 40, trig: 0, resamp: 1, decim: 0, bits: 0, ws: 0) ! 2 * 0.1

;; MiBraids ; trigger ; 28=plucked
var trig = Dust(0.6);
var pitch = TRand(45, 72, trig).rounded;
var timbre = 0.5;
var color = LfNoise1(0.3) * 0.5 + 0.5;
MiBraids(pitch: pitch, timbre: timbre, color: color, model: 28, trig: trig, resamp: 0, decim: 0, bits: 0, ws: 0) ! 2 * 0.1

;; MiBraids ; 34=kick
var trig =  Impulse(4, 0);
var pitch = LinLin(Latch(PinkNoise(), trig), -1, 1, 30, 50).rounded;
var timbre = LfNoise1(0.4) * 0.5 + 0.5;
var color = LfNoise1(0.3) * 0.5 + 0.5;
MiBraids(pitch: pitch, timbre: timbre, color: color, model: 34, trig: trig, resamp: 0, decim: 0, bits: 0, ws: 0) ! 2 * 0.2

;; MiBraids ; 34=kick ; sample rate, bit reduction and distortion
var trig =  CoinGate(0.3, Impulse(4, 0));
var decim = TRand(1, 32, trig);
var ws = LinLin(LfTri(0.2, 0), -1, 1, 0,1);
MiBraids(pitch: 40, timbre: 0.7, color: 0.7, model: 34, trig: trig, resamp: 2, decim: decim, bits: 3, ws: ws) ! 2 * 0.1