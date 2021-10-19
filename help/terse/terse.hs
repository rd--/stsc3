import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

toSt :: Int -> (String -> String) -> String -> String
toSt col trs str =
  let pad_right k n l = take n (l ++ repeat k)
      remove_newlines = filter (/= '\n')
  in if null str then "" else pad_right ' ' col str ++ remove_newlines (trs str)

main :: IO ()
main = do
  e <- fmap lines (readFile "terse.scd")
  putStrLn (unlines (map (toSt 60 Sc.stcToSt) e))
