import Data.Maybe {- base -}

import Sound.SC3.UGen.DB as Db {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Bindings.SuperCollider as Sc {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Bindings.Smalltalk as St {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as Record {- hsc3-db -}

{- | Unary operators

     Sc>>log is natural, ie. St>>ln
     Sc>>log2 is base 2, ie. St>>log2
     Sc>>log10 is base 10, ie. St>>log
-}
uop :: [(String, Int)]
uop =
  [("negated",0),("abs",5),("ceil",8),("floor",9)
  ,("frac",10),("squared",12),("cubed",13),("sqrt",14),("exp",15),("reciprocal",16),("midicps",17),("cpsmidi",18)
  ,("ln",25),("log2",26),("log",27),("sin",28),("cos",29),("tanh",36)
  ,("distort",42),("softclip",43)]

{- | Binary operators.

     Sc>>** is St>>raisedTo:
     Smalltalk modulo is rem: but use % here.
-}
binop :: [(String, Int)]
binop =
  [("+",0),("-",1),("*",2),("/",4),("%",5),("/=",7),("<",8),(">",9)
  ,("<=",10),(">=",11),("min:",12),("max:",13)
  ,("bitAnd:",14),("bitOr:",15)
  ,("lcm:",17),("gcd:",18),("roundTo:",19)
  ,("truncateTo:",21),("raisedTo:",25)
  ,("bitShiftLeft:",26),("bitShiftRight:",27)
  ,("amclip:",40),("clip2:",42),("fold2:",44)]

-- > Data.List.sort ugen == ugen
ugen :: [String]
ugen =
  ["AllpassC","AllpassL","AllpassN","AmpComp","AmpCompA","Amplitude","AnalogFoldOsc"
  ,"Balance2","BBandPass","BBandStop","Blip","BlockSize","BLowPass","BPF","BPZ2","BRF","BrownNoise","BufRd","BufWr"
  ,"ClearBuf","Clip","CombC","CombL","CombN","ControlDur","ControlRate", "Convolution","Crackle","CrossoverDistortion","CuspL"
  ,"Dbufrd","Dbufwr","Decay","Decay2","DegreeToKey","DelayC","DelayN","Demand","DetectSilence","Diwhite","Drand","Dseq","Dseries","Dshuf","Dust","Dust2","Duty"
  ,"EnvGen","ExpRand"
  ,"FBSineC", "FFT","Fold","Formant","FreqShift","FSinOsc","FreeVerb","FreeVerb2"
  ,"Gendy1","GrainFM","GrainSin","GrayNoise","GreyholeRaw","GVerb"
  ,"Hasher","HPF","HPZ1"
  ,"IFFT","Impulse","In","InFeedback","InRange","IRand","Integrator"
  ,"K2A","Klang","Klank"
  ,"LFCub","LFDNoise1","LFDNoise3","LFGauss","LFNoise0","LFNoise1","LFNoise2","LFPar","LFPulse","LFSaw","LFTri","LPF"
  ,"Lag","LagUD","Lag2","Lag3","Lag3UD","Latch","LeakDC","Limiter","Line","LinExp","LinPan2","LinRand","LinXFade2","LocalBuf","LocalIn","LocalOut"
  ,"MantissaMask","MembraneCircle","MiRings","ModDif","MoogFF","MoogLadder","MouseButton","MouseX","MouseY","MulAdd"
  ,"Normalizer"
  ,"OnePole","Out"
  ,"Pan2","Phasor","PinkNoise","Pitch","PitchShift","Pluck","Pulse","PulseCount","PulseDivider"
  ,"PV_RandComb"
  ,"RHPF","RLPF","Rand","RecordBuf","Resonz","Ringz","RunningMax"
  ,"RBezier","RDX7Env","RExpRandN","Rotate2","RRandN","RTScramble"
  ,"SampleDur","SampleRate","Saw","Select","SetBuf","SetResetFF","SinOsc","SinOscFB","Slope","Stepper","Sweep","SyncSaw"
  ,"TDuty","TExpRand","TGrains","Timer","TIRand","ToggleFF","TRand","Trig","Trig1","TwoPole","TwoZero"
  ,"VarSaw","Vibrato"
  ,"WhiteNoise","Wrap"
  ,"XFade2","XLine"]

is_osc :: Record.U -> Bool
is_osc u = (Record.u_num_inputs u > 0) && not (Record.u_is_filter u)

main :: IO ()
main = do
  St.st_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/st/SC3-UGen.st" uop binop ugen
  let col = map u_lookup_cs_err ugen
      flt = filter Record.u_is_filter col
      osc = filter is_osc col
  writeFile "/home/rohan/sw/stsc3/st/SC3-UGen-Filter.st" (St.st_filter_methods flt ++ St.st_first_input_methods osc)

-- * Sc

sc_wr :: IO ()
sc_wr = do
  let u_fm = map u_lookup_cs_err (filter (`notElem` Sc.sc_filter_method_ignore_list) ugen)
      u_fc = map u_lookup_cs_err (filter (`notElem` Sc.sc_filter_constructor_ignore_list) ugen)
      u_ir = map u_lookup_cs_err (filter (`notElem` Sc.sc_implicit_rate_ignore_list) ugen)
      u_fi = map u_lookup_cs_err (filter (`notElem` Sc.sc_first_input_ignore_list) ugen)
  writeFile "/home/rohan/sw/stsc3/sc/FilterMethods.sc" (Sc.sc_filter_methods (filter Record.u_is_filter u_fm))
  writeFile "/home/rohan/sw/stsc3/sc/OscillatorMethods.sc" (Sc.sc_first_input_methods (filter is_osc u_fi))
  writeFile "/home/rohan/sw/stsc3/sc/FilterConstructors.sc" (Sc.sc_filter_constructors (filter Record.u_is_filter u_fc))
  writeFile "/home/rohan/sw/stsc3/sc/ImplicitRateConstructors.sc" (Sc.sc_implicit_rate_constructors (filter (isNothing . Record.ugen_filter) u_ir))

{-
-- * Som
import Sound.SC3.UGen.Db.Bindings.SOM {- hsc3-db -}
som_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/lib/som/ugen" ugen
-}
