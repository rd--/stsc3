+ A { f { arg a; ^a * v } }
+ B { / { arg aNumber; ^this.div(aNumber) } + { arg aNumber; ^this.add(aNumber) } }
+ C { *a_class_method { arg a_parameter; var object = this.new; obj.f(a_parameter); ^obj } * { arg aNumber; ^this.mul(aNumber) } }
+ D { an_instance_method { ^0 } * { arg aNumber; ^this.mul(aNumber) } }
+ E { * { arg aNumber; ^this.mul(aNumber) } }

