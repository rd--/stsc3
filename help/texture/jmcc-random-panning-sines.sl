;; random panning sines (jmcc) #4
{
    var n = 8;
    {
        Pan2(
            SinOsc(80 + LinRand(0, 2000, 0), 0),
            LfNoise1(0.4 + Rand(0, 0.8)),
            LfNoise1(0.4 + Rand(0, 0.8)) * 0.4 + 0.5
        )
    }.dup(n).sum * (0.4 / n);
}.xfade(8, 8)
