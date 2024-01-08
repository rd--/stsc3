import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

main :: IO ()
main = interact (Sc.stcToJs (Just "sc.")) -- Nothing
