import Sound.SC3.UGen.DB.Bindings.Smalltalk {- hsc3-db -}

uop :: [(String, Int)]
uop =
  [("negated",0),("abs",5)
  ,("frac",10),("cubed",13),("reciprocal",16),("midicps",17)
  ,("log",25),("sin",28),("tanh",36)
  ,("distort",42)]

binop :: [(String, Int)]
binop =
  [("+",0),("-",1),("*",2),("/",4),("%",5),("/=",7),("<",8),(">",9)
  ,("<=",10),(">=",11),("min:",12),("max:",13),("lcm:",17),("gcd:",18),("round:",19)
  ,("**",25)
  ,("amclip:",40),("clip2:",42)]

-- > Data.List.sort ugen == ugen
ugen :: [String]
ugen =
  ["AllpassN","AmpComp","Amplitude","BPF","BPZ2","BrownNoise","CombC","CombL","CombN","Crackle"
  ,"Decay","Decay2","DegreeToKey","DelayN","Demand","Drand","Dseq","Dshuf","Dust","Duty"
  ,"EnvGen","ExpRand","Formant","FSinOsc","FreeVerb","GVerb","HPF","HPZ1"
  ,"IRand","Impulse","In","InFeedback","Klank"
  ,"LFNoise1","LFNoise2","LFPulse","LFSaw","LFTri","LPF"
  ,"Lag","LagUD","Lag2","Lag3","Latch","LeakDC","Line","LinExp","LocalBuf","LocalIn","LocalOut"
  ,"ModDif","MouseX","MouseY","MulAdd","OnePole","Out"
  ,"Pan2","Phasor","PinkNoise","Pluck","Pulse","PulseDivider"
  ,"RHPF","RLPF","Rand","Resonz","Ringz","RunningMax"
  ,"SampleDur","Saw","Select","SetBuf","SetResetFF","SinOsc","SinOscFB","Sweep"
  ,"Trig","VarSaw","WhiteNoise","XFade2","XLine"]

main :: IO ()
main = st_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/st/SC3-UGen.st" uop binop ugen
