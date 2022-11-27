;; Line ; note Line is an existing class in Squeak...
var f = Line(200, 17000, 5, 1);
SinOsc(f, 0) * 0.05

;; Line ; self deleting
var a = Line(0.1, 0, 1, 2);
SinOsc(440, 0) * a
