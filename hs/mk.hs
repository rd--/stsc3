import Sound.SC3.UGen.DB.Bindings.Smalltalk {- hsc3-db -}

uop :: [(String, Int)]
uop =
  [("negated",0),("abs",5),("ceil",8),("floor",9)
  ,("frac",10),("cubed",13),("reciprocal",16),("midicps",17)
  ,("log",25),("sin",28),("cos",29),("tanh",36)
  ,("distort",42)]

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
  ["AllpassN","AmpComp","Amplitude"
  ,"Blip","BLowPass","BPF","BPZ2","BrownNoise"
  ,"Clip","CombC","CombL","CombN","ControlDur","Crackle"
  ,"Decay","Decay2","DegreeToKey","DelayN","Demand","Diwhite","Drand","Dseq","Dshuf","Dust","Dust2","Duty"
  ,"EnvGen","ExpRand"
  ,"Formant","FSinOsc","FreeVerb"
  ,"Gendy1","GrainFM","GreyholeRaw","GVerb"
  ,"Hasher","HPF","HPZ1"
  ,"IRand","Impulse","In","InFeedback"
  ,"Klank"
  ,"LFCub","LFNoise0","LFNoise1","LFNoise2","LFPulse","LFSaw","LFTri","LPF"
  ,"Lag","LagUD","Lag2","Lag3","Latch","LeakDC","Limiter","Line","LinExp","LocalBuf","LocalIn","LocalOut"
  ,"MiRings","ModDif","MouseX","MouseY","MulAdd","OnePole","Out"
  ,"Pan2","Phasor","PinkNoise","PitchShift","Pluck","Pulse","PulseCount","PulseDivider"
  ,"RHPF","RLPF","Rand","Resonz","Ringz","RunningMax"
  ,"SampleDur","SampleRate","Saw","Select","SetBuf","SetResetFF","SinOsc","SinOscFB","Sweep"
  ,"TDuty","TExpRand","Timer","TIRand","TRand","Trig","Trig1"
  ,"VarSaw"
  ,"WhiteNoise"
  ,"XFade2","XLine"]

main :: IO ()
main = st_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/st/SC3-UGen.st" uop binop ugen

{-
import Sound.SC3.UGen.DB.Bindings.SOM {- hsc3-db -}
som_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/som/SC3-UGen.som" ugen
-}
