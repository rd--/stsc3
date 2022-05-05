import qualified Language.Smalltalk.FileOut as St {- stsc3 -}

import qualified Language.Smalltalk.SuperCollider.Ast as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

-- | Parse class library file, a sequence of class definitions.
stc_parse_class_definition_seq :: String -> [Sc.ScClassDefinition]
stc_parse_class_definition_seq = Sc.superColliderParserClassDefinitionSeq . Sc.alexScanTokens

stsc3_psuedo_ugens_source_file :: FilePath
stsc3_psuedo_ugens_source_file = "/home/rohan/sw/stsc3/st/Sc3-Ugen-Pseudo.stc"

main :: IO ()
main = do
  txt <- readFile stsc3_psuedo_ugens_source_file
  let cd = stc_parse_class_definition_seq txt
  mapM_ (putStrLn . St.fileOutClassDefinition . Sc.scClassDefinitionToSt) cd
