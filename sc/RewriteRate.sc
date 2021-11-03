+ UGen { kr { (this.rate == 'audio').if({ this.rate = 'control'; this.inputs.kr; }) } }
+ OutputProxy { kr { this.source.kr } }
+ Array { kr { this.do({ arg item; item.kr }) } }
+ SimpleNumber { kr { } }
