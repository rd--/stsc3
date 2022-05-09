import qualified Language.Smalltalk.FileOut as St {- stsc3 -}

import qualified Language.Smalltalk.SuperCollider.Ast as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

-- | Parse class library file, a sequence of class definitions.
stc_parse_class_definition_seq :: String -> [Sc.ScClassDefinition]
stc_parse_class_definition_seq =
  let f cd = cd {Sc.classCategory = Just "Sound-Sc3"}
  in map f . Sc.superColliderParserClassDefinitionSeq . Sc.alexScanTokens

stsc3_dir :: FilePath
stsc3_dir = "/home/rohan/sw/stsc3/"

stsc3_file :: FilePath -> FilePath
stsc3_file = (++) stsc3_dir

main :: IO ()
main = do
  stc_text <- readFile (stsc3_file "st/Sc3-Ugen-Pseudo.stc")
  let cd = stc_parse_class_definition_seq stc_text
      st_text = unlines (map (St.fileOutClassDefinition . Sc.scClassDefinitionToSt) cd)
  writeFile (stsc3_file "st/Sc3-Ugen-Pseudo.st") st_text
