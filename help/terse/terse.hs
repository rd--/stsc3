import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

import qualified Sound.SC3.Lisp.SuperCollider as Lisp {- hsc3-lisp -}

sideBySide :: Int -> (String -> String) -> String -> String
sideBySide col trs str =
  let pad_right k n l = take n (l ++ repeat k)
      remove_newlines = filter (/= '\n')
  in if null str then "" else pad_right ' ' col str ++ remove_newlines (trs str)

rwToLisp :: IO ()
rwToLisp = do
  e <- fmap lines (readFile "terse.scd")
  putStrLn (unlines (map (sideBySide 60 Lisp.scToLispViewer) e))

rwToSt :: IO ()
rwToSt = do
  e <- fmap lines (readFile "terse.scd")
  putStrLn (unlines (map (sideBySide 60 Sc.stcToSt) e))
