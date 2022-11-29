;; GrainSin
var overlap = MouseY(0, 2, 0, 0.2);
var f = MouseX(1, 220, 0, 0.2);
var t = Impulse(f, 0);
var a = GrainSin(2, t, overlap / f, 440, 0, -1, 512);
LeakDc(a, 0.995) * 0.1

;; GrainSin ; f0 https://www.listarc.bham.ac.uk/lists/sc-users/msg66911.html ; requires=kr
var k = 16;
var t = Impulse(MouseY(1, 999, 0, 0.2), 0);
var f = { :i | i ** LfNoise0(1).range(0.3, 0.7) + 1 * 99 };
var l = (0 .. k - 1).collect(f);
GrainSin(2, t, MouseX(0.001, 0.5, 1, 0.2), TChoose(t, l.kr), 0, -1, 512) * 0.1