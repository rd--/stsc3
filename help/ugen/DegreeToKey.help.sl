(* DegreeToKey *)
var b = [0 2 4 5 7 9 11].asLocalBuf;
var m = DegreeToKey(b, [MouseX(0, 15, 0, 0.2), MouseY(3, 10, 0, 0.2)], 12);
CombC(Blip(Lag(([48, 36] + m).MidiCps, 0.01), [2, 3]), 0.2, 0.2, 2) * [0.05, 0.1]
