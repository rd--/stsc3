{-
The Sc2 example files by James McCartney.
Here they are fragmented into distinct files under stc3/help/graphs/.
The first entry in each file should be the closest to the original definition, in approach and layout, other variants may follow.
This file indexes the fragments.
-}

jmcc_sc2_examples :: [(Int, [String])]
jmcc_sc2_examples =
  [(1
   ,["analog bubbles"
    ,"lfo modulation"
    ,"moto rev"
    ,"scratchy"
    ,"sprinkler"
    ,"sprinkler mouse"
    ,"harmonic swimming"
    ,"harmonic tumbling"
    ,"hell is busy"
    ,"pond life"
    ,"alien froggies"
    ,"random sine waves"
    ,"random pulsations"
    ,"tremulate"
    ,"reso pulse"
    ])
  ,(2
   ,["bouncing objects"
    ,"rocks on rails"
    ,"pulsing bottles"
    ,"what was I thinking?"
    ,"police state"
    ])]

space_to_hyphen :: Char -> Char
space_to_hyphen c = if c == ' ' then '-' else c

derive_file_name :: String -> FilePath
derive_file_name nm = "jmcc-" ++ map space_to_hyphen nm ++ ".stc"
