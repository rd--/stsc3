A { }
B : A { }
C : A { var z; }
D : A { var z; m { } }
E : A { var y, z; m { } n { } }
F : A { classvar c; }
G : A { classvar c; *m { } }
H : A { classvar y, z; var x, w; m { arg n; ^ x * n } }
I : A { classvar y, z; var x, w; *c { arg d; ^ z + d } m { arg n; ^ x * n } }
J : A { + { arg x; ^this.add(x) } * { arg x; ^this.mul(x) } / { arg x; ^this.div(x) } }
K : B { - { arg x; ^this - x } negate { arg x; ^0 - this } }
L { stringWithoutNewline { ^"" } literalArray { ^#[] } }
M { var p = 1, q = 'two', r = #[3.0, $4]; s { ^[p, q, r] } }
