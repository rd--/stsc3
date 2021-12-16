+ UGen { kr { (this.rate == 'audio').if({ this.rate = 'control'; this.inputs.kr; }); ^this } }
+ OutputProxy { kr { this.source.kr; ^this } }
+ Array { kr { this.do({ arg item; item.kr }); ^this } }
+ SimpleNumber { kr { ^this } }
