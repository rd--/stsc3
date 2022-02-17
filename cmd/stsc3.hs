import Control.Monad {- base -}
import System.Environment {- base -}
import System.IO {- base -}

import qualified Control.Monad.Loops as Loop {- monad-loops -}

import qualified Sound.SC3.Common.Help as Help {- hsc3 -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Lexer as St.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Parser as St.Parser {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ndef as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}


-- | Parse and then pretty print Smalltalk program.
st_cat_parsec :: String -> String
st_cat_parsec = St.smalltalkProgram_pp . St.stParse St.smalltalkProgram

st_cat_happy :: String -> String
st_cat_happy = St.smalltalkProgram_pp . St.Parser.smalltalkParser . St.Lexer.alexScanTokens

st_cat :: String -> FilePath -> IO ()
st_cat which fn = do
  st <- readFile fn
  putStrLn ((if which == "parsec" then st_cat_parsec else st_cat_happy) st)

sc_cat :: FilePath -> IO ()
sc_cat fn = do
  txt_fragments <- Help.read_file_fragments fn
  let expr_fragments = map (Sc.superColliderParser . Sc.alexScanTokens) txt_fragments
  mapM_ (putStrLn . Sc.scInitializerDefinitionPrint) expr_fragments

{-
-- | Fragment input file and run stcToJs at each fragment.
stc_to_js :: FilePath -> IO ()
stc_to_js fn = do
  txt_fragments <- Help.read_file_fragments fn
  mapM_ (putStrLn . Sc.stcToJs) txt_fragments
-}

-- | Like interact but with named files.
procFile :: FilePath -> FilePath -> (String -> String) -> IO ()
procFile inFile outFile strFunc = do
  txt <- readFile inFile
  writeFile outFile (strFunc txt)

getLineIfReady :: IO (Maybe String)
getLineIfReady = do
  r <- hReady stdin
  if r then fmap Just (hGetLine stdin) else return Nothing

getAvailable :: IO [String]
getAvailable = do
  _ <- hWaitForInput stdin (-1)
  Loop.unfoldM getLineIfReady

procStdio :: (String -> String) -> IO ()
procStdio strFunc = forever (getAvailable >>= \ln -> hPutStrLn stdout (strFunc (unlines ln)) >> hFlush stdout)

help :: [String]
help =
    ["stsc3 command [arguments]"
    ," sc cat fragment supercollider-program-file..."
    ," st cat {parsec|happy} smalltalk-program-file..."
    ," rewrite ndef"
    ," translate [stream] {sc | stc} { st | js } [input-file output-file]"
    ]

main :: IO ()
main = do
  a <- getArgs
  let trs in_ty out_ty = if in_ty == "stc" then (if out_ty == "st" then Sc.stcToSt else Sc.stcToJs) else Sc.scToSt
  case a of
    "sc":"cat":"fragment":fn_seq -> mapM_ (\fn -> putStrLn fn >> sc_cat fn) fn_seq
    "st":"cat":which:fn_seq -> mapM_ (\fn -> putStrLn fn >> st_cat which fn) fn_seq
    ["rewrite","ndef"] -> interact Sc.stcUgenToNdef
    ["translate",in_ty,out_ty] -> interact (trs in_ty out_ty)
    ["translate",in_ty,out_ty,inFile,outFile] -> procFile inFile outFile (trs in_ty out_ty)
    ["translate","stream",in_ty,out_ty] -> procStdio (trs in_ty out_ty)
    _ -> putStrLn (unlines help)
