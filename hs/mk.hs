import Sound.SC3.UGen.DB.Bindings.Smalltalk {- hsc3-db -}

{- | Unary operators

     Sc>>log is natural, ie. St>>ln
     Sc>>log2 is base 2, ie. St>>log2
     Sc>>log10 is base 10, ie. St>>log
-}
uop :: [(String, Int)]
uop =
  [("negated",0),("abs",5),("ceil",8),("floor",9)
  ,("frac",10),("cubed",13),("exp",15),("reciprocal",16),("midicps",17)
  ,("ln",25),("log2",26),("log",27),("sin",28),("cos",29),("tanh",36)
  ,("distort",42)]

{- | Binary operators.

     Sc>>** is St>>raisedTo:
     Smalltalk modulo is rem: but use % here.
-}
binop :: [(String, Int)]
binop =
  [("+",0),("-",1),("*",2),("/",4),("%",5),("/=",7),("<",8),(">",9)
  ,("<=",10),(">=",11),("min:",12),("max:",13)
  ,("bitAnd:",14),("bitOr:",15)
  ,("lcm:",17),("gcd:",18),("round:",19)
  ,("raisedTo:",25)
  ,("bitShiftLeft:",26),("bitShiftRight:",27)
  ,("amclip:",40),("clip2:",42)]

-- > Data.List.sort ugen == ugen
ugen :: [String]
ugen =
  ["AllpassL","AllpassN","AmpComp","AmpCompA","Amplitude"
  ,"BBandPass","Blip","BLowPass","BPF","BPZ2","BRF","BrownNoise","BufRd","BufWr"
  ,"ClearBuf","Clip","CombC","CombL","CombN","ControlDur","Crackle","CuspL"
  ,"Dbufrd","Dbufwr","Decay","Decay2","DegreeToKey","DelayN","Demand","DetectSilence","Diwhite","Drand","Dseq","Dshuf","Dust","Dust2","Duty"
  ,"EnvGen","ExpRand"
  ,"Formant","FSinOsc","FreeVerb"
  ,"Gendy1","GrainFM","GreyholeRaw","GVerb"
  ,"Hasher","HPF","HPZ1"
  ,"IRand","Impulse","In","InFeedback"
  ,"K2A","Klank"
  ,"LFCub","LFNoise0","LFNoise1","LFNoise2","LFPar","LFPulse","LFSaw","LFTri","LPF"
  ,"Lag","LagUD","Lag2","Lag3","Latch","LeakDC","Limiter","Line","LinExp","LocalBuf","LocalIn","LocalOut"
  ,"MiRings","ModDif","MoogFF","MoogLadder","MouseX","MouseY","OnePole","Out"
  ,"Pan2","Phasor","PinkNoise","PitchShift","Pluck","Pulse","PulseCount","PulseDivider"
  ,"RHPF","RLPF","Rand","RecordBuf","Resonz","Ringz","RunningMax"
  ,"SampleDur","SampleRate","Saw","Select","SetBuf","SetResetFF","SinOsc","SinOscFB","Sweep"
  ,"TDuty","TExpRand","TGrains","Timer","TIRand","TRand","Trig","Trig1"
  ,"VarSaw","Vibrato"
  ,"WhiteNoise"
  ,"XFade2","XLine"]

main :: IO ()
main = st_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/st/SC3-UGen.st" uop binop ugen

{-
import Sound.SC3.UGen.DB.Bindings.SOM {- hsc3-db -}
som_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/lib/som/ugen" ugen
-}
