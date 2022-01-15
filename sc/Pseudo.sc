+ AudioIn { *new { arg channelArray; ^SoundIn.ar(channelArray - 1) } }
+ Changed { *new { arg input, threshold; ^Changed.performList(input.rate.rateToSelector, [input, threshold]) } }
+ SelectX { *new { arg which, array; ^SelectX.ar(which, array) } }

Splay2 { *new { arg inArray; ^Splay.ar(inArray, 1, 1, 0, true) } }

