+ AudioIn { *new { arg channelArray; ^SoundIn.ar(channelArray - 1) } }
+ Changed { *new { arg input, threshold; ^Changed.performList(input.rate.rateToSelector, [input, threshold]) } }
+ LinLin { *new { arg in, srclo, srchi, dstlo, dsthi; ^LinLin.performList(in.rate.rateToSelector, [in, srclo, srchi, dstlo, dsthi]) } }
+ PMOsc { *new { arg carfreq, modfreq, pmindex, modphase; ^PMOsc.ar(carfreq, modfreq, pmindex, modphase) } }
+ SelectX { *new { arg which, array; ^SelectX.ar(which, array) } }
+ TChoose { *new { arg trig, array; ^TChoose.performList(trig.rate.rateToSelector, [trig, array]) } }
