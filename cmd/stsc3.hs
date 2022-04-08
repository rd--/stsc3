import System.Environment {- base -}

import qualified Music.Theory.IO {- hmt-base -}

import qualified Sound.SC3.Common.Help as Help {- hsc3 -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Lexer as St.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Parser as St.Parser {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

import qualified Language.Smalltalk.SuperCollider.Ast as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ndef as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

-- | Parse and then pretty print Smalltalk program, using Parsec parser.
st_cat_parsec :: String -> String
st_cat_parsec = St.smalltalkProgram_pp . St.stParse St.smalltalkProgram

-- | Parse and then pretty print Smalltalk program, using Alex/Happy parser.
st_cat_happy :: String -> String
st_cat_happy = St.smalltalkProgram_pp . St.Parser.smalltalkParser . St.Lexer.alexScanTokens

-- | Parse and then pretty print Smalltalk program, using indicated parser.
st_cat :: String -> FilePath -> IO ()
st_cat which fn = do
  st <- readFile fn
  putStrLn ((if which == "parsec" then st_cat_parsec else st_cat_happy) st)

-- | Parse and then pretty print SuperCollider program.
sc_cat_fragments :: FilePath -> IO ()
sc_cat_fragments fn = do
  txt_fragments <- Help.read_file_fragments fn
  let expr_fragments = map (Sc.superColliderParserInitializerDefinition . Sc.alexScanTokens) txt_fragments
  mapM_ (putStrLn . Sc.scInitializerDefinitionPrint) expr_fragments

-- | Parse class library file, a sequence of class definitions.
sc_parse_class_definition_seq :: String -> [Sc.ScClassDefinition]
sc_parse_class_definition_seq = Sc.superColliderParserClassDefinitionSeq . Sc.alexScanTokens

{- | Read and print library.

sc_cat_library "/home/rohan/sw/stsc3/help/expr/library.sc"
sc_cat_library "/home/rohan/rd/j/2022-04-08/before-pim.sc"
-}
sc_cat_library :: FilePath -> IO ()
sc_cat_library fn =
  let rw = putStrLn . unlines . map Sc.scClassDefinitionPrint . sc_parse_class_definition_seq
  in rw =<< readFile fn

{-
-- | Fragment input file and run stcToJs at each fragment.
stc_to_js :: FilePath -> IO ()
stc_to_js fn = do
  txt_fragments <- Help.read_file_fragments fn
  mapM_ (putStrLn . Sc.stcToJs) txt_fragments
-}

help :: [String]
help =
    ["stsc3 command [arguments]"
    ," sc cat { fragment | library } supercollider-file..."
    ," st cat { parsec | happy } smalltalk-file..."
    ," rewrite ndef"
    ," translate [ stream ] { sc | stc } { js | sc | scm | st } [ input-file output-file ]"
    ]

main :: IO ()
main = do
  a <- getArgs
  let trs in_ty out_ty =
        case (in_ty, out_ty) of
          ("stc", "st") -> Sc.stcToSt
          ("stc", "js") -> Sc.stcToJs
          ("stc", "sc") -> Sc.stcToSc
          ("stc", "scm") -> Sc.stcToScheme
          ("sc", "st") -> Sc.scToSt
          _ -> error "stsc3: unknown translation"
  case a of
    "sc":"cat":"fragment":fn_seq -> mapM_ (\fn -> putStrLn fn >> sc_cat_fragments fn) fn_seq
    "sc":"cat":"library":fn_seq -> mapM_ (\fn -> putStrLn fn >> sc_cat_library fn) fn_seq
    "st":"cat":which:fn_seq -> mapM_ (\fn -> putStrLn fn >> st_cat which fn) fn_seq
    ["rewrite","ndef"] -> interact Sc.stcUgenToNdef
    ["translate",in_ty,out_ty] -> interact (trs in_ty out_ty)
    ["translate",in_ty,out_ty,inFile,outFile] -> Music.Theory.IO.interactWithFiles inFile outFile (trs in_ty out_ty)
    ["translate","stream",in_ty,out_ty] -> Music.Theory.IO.interactWithStdio (trs in_ty out_ty)
    _ -> putStrLn (unlines help)
