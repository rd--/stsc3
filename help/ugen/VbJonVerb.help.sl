(* VbJonVerb *)
var freq = [1, 2.1, 2.9].collect { :item |
	item * 1000
};
var src = Resonz({ Dust(0.25) } ! 3, freq, 0.01).Sum * 10;
VbJonVerb(src, 0.8, 0.3, 0.8, 0.1, 0.5) + src

(* VbJonVerb ; default param *)
VbJonVerb(Dust(2), 0.7, 0.3, 0.8, 0.5, 0.5)
