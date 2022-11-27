;; Changed ; simple composition of HPZ1 and > (pseudo ugen)
var s = LFNoise0(2);
var c = Changed(s, 0);
var d = Decay2(c, 0.01, 0.5);
SinOsc(440 + ([s, d] * 440), 0) * 0.1

;; Changed ; SinOsc is constantly changing
var s = SinOsc(440, 0);
var c = Changed(s, 0);
s * c * 0.1

;; Changed ; fixed number of sample impulses ; https://fredrikolofsson.com/f0blog/impulse-train/
var dur = 1;
var num = 8;
Changed(Sweep(0, num / dur).min(num).ceiling, 0)
