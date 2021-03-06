import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import Sound.Sc3.Ugen.Db as Db {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Bindings.Js as Js {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Bindings.Som as Som {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Bindings.SuperCollider as Sc {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Bindings.Smalltalk as St {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Record as Record {- hsc3-db -}

{- | Unary operators

     Sc>>log is natural, ie. St>>ln
     Sc>>log2 is base 2, ie. St>>log2
     Sc>>log10 is base 10, ie. St>>log
-}
uop :: [(String, Int)]
uop =
  [("negated",0),("abs",5),("ceil",8),("floor",9)
  ,("frac",10), ("sign", 11), ("squared",12),("cubed",13),("sqrt",14),("exp",15),("reciprocal",16)
  ,("midiCps",17),("cpsMidi",18), ("midiRatio", 19), ("ratioMidi", 20)
  ,("dbAmp", 21), ("ampDb", 22), ("ln",25),("log2",26),("log",27),("sin",28),("cos",29),("tanh",36)
  ,("distort",42),("softClip",43)]

{- | Binary operators.

     Sc>>** is not St>>raisedTo: since in Sc (0 ** 0) is 1 and in St (0 raisedTo: 0) is an error
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
  ,("amClip:",40),("clip2:",42),("fold2:",44)]

{-
> selectorToSpecialIndex uop
> selectorToSpecialIndex binop
-}
selectorToSpecialIndex :: [(String, Int)] -> String
selectorToSpecialIndex =
  let f (sel, ix) = printf "#%s -> %d" sel ix
  in intercalate ". " . map f

{-
> Data.List.sort ugen == ugen
> filter (not . Db.ugen_is_core) ugen
-}
ugen :: [String]
ugen =
  ["AllpassC","AllpassL","AllpassN","AmpComp","AmpCompA","Amplitude"
  ,"Balance2","BBandPass","BBandStop","BHiPass","Blip","BlockSize","BLowPass","BPeakEQ","BPF","BPZ2","BRF","BRZ2","BrownNoise","BufDur","BufFrames","BufRateScale","BufRd","BufSampleRate","BufWr"
  ,"ClearBuf","Clip","ClipNoise","CoinGate","CombC","CombL","CombN","ControlDur","ControlRate","Convolution","Crackle","CuspL"
  ,"Dbufrd","Dbufwr","DC","Decay","Decay2","DegreeToKey","Delay1","Delay2","DelayC","DelayL","DelayN","Demand","DetectSilence","Diwhite","Drand","Dseq","Dseries","Dshuf","Dust","Dust2","Duty","DWGPluckedStiff","Dxrand"
  ,"EnvGen","ExpRand"
  ,"FBSineL","FBSineC","FFT","FM7", "Fold","Formant","Formlet","FOS","FreqShift","FSinOsc","FreeVerb","FreeVerb2"
  ,"Gate","Gendy1","GrainFM","GrainSin","GrayNoise","GVerb"
  ,"Hasher","HenonL","HenonC","HPF","HPZ1","HPZ2"
  ,"IFFT","Impulse","In","Index","IndexInBetween","InFeedback","InRange","IRand","Integrator"
  ,"K2A","KeyState","Klang","Klank"
  ,"LFBrownNoise1","LFClipNoise","LFCub","LFDNoise1","LFDNoise3","LFGauss","LFNoise0","LFNoise1","LFNoise2","LFPar","LFPulse","LFSaw","LFTri","LPF"
  ,"Lag","LagUD","Lag2","Lag3","Lag3UD","Latch","LatoocarfianC","LeakDC","Limiter","LinCongC","Line","Linen","LinExp","LinPan2","LinRand","LinXFade2","LocalBuf","LocalIn","LocalOut","LorenzL","LPZ1","LPZ2"
  ,"MantissaMask","MaxLocalBufs","Median","MidEQ","ModDif","MoogFF","MouseButton","MouseX","MouseY","MulAdd"
  ,"Normalizer","NRand","NumOutputBuses"
  ,"OnePole","OneZero","Osc","Out"
  ,"Pan2","PanAz","PanB","PeakFollower","Perlin3", "Phasor","PinkNoise","Pitch","PitchShift","PlayBuf","Pluck","Pulse","PulseCount","PulseDivider"
  ,"PV_RandComb"
  ,"QuadL","QuadC"
  ,"RHPF","RLPF","Rand","RecordBuf","ReplaceOut","Resonz","Ringz","RunningMax","RunningSum"
  ,"Rotate2"
  ,"SampleDur","SampleRate","Sanitize","Saw","Schmidt","Select","SetBuf","SetResetFF","SinOsc","SinOscFB","Slew","Slope","SOS","StandardL","Stepper","Sweep","SyncSaw"
  ,"TDuty","TExpRand","TGrains","Timer","TIRand","ToggleFF","TRand","Trig","Trig1","TwoPole","TwoZero"
  ,"VarSaw","VBJonVerb", "Vibrato"
  ,"WaveLoss","WhiteNoise","Wrap","WrapIndex"
  ,"XFade2","XLine"
  ,"ZeroCrossing"
  ,"MoogLadder" -- sc3-plugins/Bhob
  ,"GreyholeRaw" -- sc3-plugins/DEIND
  ,"CrossoverDistortion" -- sc3-plugins/Distortion
  ,"Friction" -- sc3-plugins/MCLD
  ,"MembraneCircle" -- sc3-plugins/Membrane
  ,"VOSIM" -- sc3-plugins/VOSIM
  ,"MiBraids", "MiClouds", "MiRings" -- mi-UGens
  ,"AnalogFoldOsc" -- portedplugins
  ,"RCD","SCM" -- vb_UGens
  ,"DustRange","ExpRandN","LinRandN","RandN" -- sc3-rdu
  ,"TScramble" -- sc3-rdu
  ,"Dx7","Dx7Env","ObxdFilter","SvfBp","SvfHp","SvfLp" -- sc3-rdu
  ,"Bezier","Freezer" -- sc3-rdu ,"ShufflerB"
  ]

is_osc :: Record.U -> Bool
is_osc u = (Record.u_num_inputs u > 0) && not (Record.u_is_filter u)

main :: IO ()
main = do
  Som.som_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/som/Sc3/Ugen/" ugen
  St.st_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/st/Sc3-Ugen-Bindings.st" uop binop ugen
  Js.js_sc3_gen_bindings_wr True "/home/rohan/sw/jssc3/js/sc3-bindings.ts" Js.js_sc3_uop Js.js_sc3_binop ugen
  let col = map u_lookup_cs_err ugen
      flt = filter Record.u_is_filter col
      osc = filter is_osc col
  writeFile "/home/rohan/sw/stsc3/st/Sc3-Ugen-Filter.st" (St.st_filter_methods flt ++ St.st_first_input_methods osc)

-- * Sc

sc_wr :: IO ()
sc_wr = do
  let u_fc = map u_lookup_cs_err (filter (`notElem` Sc.sc_filter_constructor_ignore_list) ugen)
      u_ir = map u_lookup_cs_err (filter (`notElem` Sc.sc_implicit_rate_ignore_list) ugen)
      --u_fm = map u_lookup_cs_err (filter (`notElem` Sc.sc_filter_method_ignore_list) ugen)
      --u_fi = map u_lookup_cs_err (filter (`notElem` Sc.sc_first_input_ignore_list) ugen)
  --writeFile "/home/rohan/sw/stsc3/sc/FilterMethods.sc" (Sc.sc_filter_methods (filter Record.u_is_filter u_fm))
  --writeFile "/home/rohan/sw/stsc3/sc/OscillatorMethods.sc" (Sc.sc_first_input_methods (filter is_osc u_fi))
  writeFile "/home/rohan/sw/stsc3/sc/FilterConstructors.sc" (Sc.sc_filter_constructors (filter Record.u_is_filter u_fc))
  writeFile "/home/rohan/sw/stsc3/sc/ImplicitRateConstructors.sc" (Sc.sc_implicit_rate_constructors (filter (isNothing . Record.ugen_filter) u_ir))

{-
-}
