import System.FilePath {- filepath -}

import Sound.Sc3.Data.Text.Markdown {- hsc3-data -}

sc2_ugen_cat :: [String]
sc2_ugen_cat = words "analysis buffers controls delays envelopes events filters misc noise oscillators panners random samples triggers unary-operators"

sc2_ugen_dir :: String
sc2_ugen_dir = "/home/rohan/sw/stsc3/help/sc2/help/unit-generators/"

sc3_ugen_rw :: FilePath -> FilePath -> IO ()
sc3_ugen_rw dir cat = do
  let in_fn = dir </> cat <.> "md"
      gen_fn wd = dir </> cat </> head wd <.> "help.md"
  md_extract_h1 in_fn gen_fn

{-
mapM_ (sc3_ugen_rw sc2_ugen_dir) sc2_ugen_cat
-}
