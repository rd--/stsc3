import qualified Language.Smalltalk.Spl.Translate as Sc {- stsc3 -}

main :: IO ()
main = interact (Sc.stcToJs (Just "sc.")) -- Nothing
