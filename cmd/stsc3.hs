import Control.Monad {- base -}
import System.Environment {- base -}
import System.IO {- base -}

import qualified Control.Monad.Loops as Loop {- monad-loops -}

import qualified Sound.SC3 as SC3 {- hsc3 -}
import qualified Sound.SC3.Common.Help as Help {- hsc3 -}

{-
import qualified Sound.SC3.UGen.Dot as Dot {- hsc3-dot -}
-}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Lexer as St.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Parser as St.Parser {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

import qualified Language.Smalltalk.Som as Som {- stsc3 -}

import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ndef as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

{-
import qualified Interpreter.Lisp.Ansi {- stsc3 -}
import qualified Interpreter.Lisp.Expr {- stsc3 -}
-}
import qualified Interpreter.Som.Repl {- stsc3 -}

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
stsc3_play :: (FilePath -> IO SC3.UGen) -> FilePath -> IO ()
stsc3_play evalFile fn = evalFile fn >>= SC3.audition

stsc3_draw :: (FilePath -> IO SC3.UGen) -> FilePath -> IO ()
stsc3_draw evalFile fn = evalFile fn >>= Dot.draw . SC3.out 0
-}

help :: [String]
help =
    ["stsc3 command [arguments]"
    ," {draw|play} {ansi|expr} file"
    ," sc cat fragment supercollider-program-file..."
    ," st cat {parsec|happy} smalltalk-program-file..."
    ," repl {ansi|expr|som}"
    ," rewrite ndef"
    ," run som class arguments..."
    ," translate [stream] {sc | stc} st [input-file output-file]"
    ]

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

main :: IO ()
main = do
  somDirectory <- Som.somSystemClassPath
  a <- getArgs
  case a of
    "sc":"cat":"fragment":fn_seq -> mapM_ (\fn -> putStrLn fn >> sc_cat fn) fn_seq
    "st":"cat":which:fn_seq -> mapM_ (\fn -> putStrLn fn >> st_cat which fn) fn_seq
{-
    ["repl","ansi"] -> Interpreter.Lisp.Ansi.replMain
    ["repl","expr"] -> Interpreter.Lisp.Expr.replMain
-}
    ["repl","som"] -> Interpreter.Som.Repl.replMain somDirectory
    ["rewrite","ndef"] -> interact Sc.stcUgenToNdef
    "run":"som":cl:arg -> Interpreter.Som.Repl.loadAndRunClass somDirectory cl arg
{-
    ["draw","ansi",fn] -> stsc3_draw Interpreter.Lisp.Ansi.evalSmalltalkFile fn
    ["draw","expr",fn] -> stsc3_draw Interpreter.Lisp.Expr.evalSmalltalkFile fn
    ["play","ansi",fn] -> stsc3_play Interpreter.Lisp.Ansi.evalSmalltalkFile fn
    ["play","expr",fn] -> stsc3_play Interpreter.Lisp.Expr.evalSmalltalkFile fn
-}
    ["stop"] -> SC3.withSC3 SC3.reset
    ["translate",ty,"st"] -> interact (if ty == "stc" then Sc.stcToSt else Sc.scToSt)
    ["translate",ty,"st",inFile,outFile] -> procFile inFile outFile (if ty == "stc" then Sc.stcToSt else Sc.scToSt)
    ["translate","stream",ty,"st"] -> procStdio (if ty == "stc" then Sc.stcToSt else Sc.scToSt)
    _ -> putStrLn (unlines help)
