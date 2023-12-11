(* TGrains ; requires=SfAcquire *)
var numChannels = 2;
var sf = SfAcquireMono('crotale-d6');
var triggerRate = MouseY(2, 120, 1, 0.2);
var trigger = Impulse(triggerRate, 0);
var rate = 1.2 ^ (WhiteNoise() * 3).RoundTo(1);
var centerPos = MouseX(0, SfDur(sf), 0, 0.2);
var dur = 1.2 / triggerRate;
var pan = TRand(-1, 1, trigger);
var amp = 1 / 2;
var interp = 4;
TGrains(numChannels, trigger, sf, rate, centerPos, dur, pan, amp, interp)
