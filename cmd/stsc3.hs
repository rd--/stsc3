import System.Environment {- base -}

import qualified Sound.SC3 as SC3 {- hsc3 -}

import qualified Sound.SC3.UGen.Dot as Dot {- hsc3-dot -}

import qualified Sound.SC3.Graphs.Polyglot as Polyglot {- hsc3-graphs -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Lexer as St.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Parser as St.Parser {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

import qualified Language.Smalltalk.Som as Som {- stsc3 -}

import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}

import qualified Interpreter.Lisp.Ansi {- stsc3 -}
import qualified Interpreter.Lisp.Expr {- stsc3 -}
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
  txt_fragments <- Polyglot.read_file_fragments fn
  let expr_fragments = map (Sc.superColliderParser . Sc.alexScanTokens) txt_fragments
  mapM_ (putStrLn . Sc.scInitializerDefinitionPrint) expr_fragments

stsc3_play :: (FilePath -> IO SC3.UGen) -> FilePath -> IO ()
stsc3_play evalFile fn = evalFile fn >>= SC3.audition

stsc3_draw :: (FilePath -> IO SC3.UGen) -> FilePath -> IO ()
stsc3_draw evalFile fn = evalFile fn >>= Dot.draw . SC3.out 0

help :: [String]
help =
    ["stsc3 command [arguments]"
    ," {draw|play} {ansi|expr} file"
    ," sc cat fragment supercollider-program-file..."
    ," st cat {parsec|happy} smalltalk-program-file..."
    ," repl {ansi|expr|som}"
    ," run som class arguments..."
    ]

main :: IO ()
main = do
  somDirectory <- Som.somSystemClassPath
  a <- getArgs
  case a of
    "sc":"cat":"fragment":fn_seq -> mapM_ (\fn -> putStrLn fn >> sc_cat fn) fn_seq
    "st":"cat":which:fn_seq -> mapM_ (\fn -> putStrLn fn >> st_cat which fn) fn_seq
    ["repl","ansi"] -> Interpreter.Lisp.Ansi.replMain
    ["repl","expr"] -> Interpreter.Lisp.Expr.replMain
    ["repl","som"] -> Interpreter.Som.Repl.replMain somDirectory
    "run":"som":cl:arg -> Interpreter.Som.Repl.loadAndRunClass somDirectory cl arg
    ["draw","ansi",fn] -> stsc3_draw Interpreter.Lisp.Ansi.evalSmalltalkFile fn
    ["draw","expr",fn] -> stsc3_draw Interpreter.Lisp.Expr.evalSmalltalkFile fn
    ["play","ansi",fn] -> stsc3_play Interpreter.Lisp.Ansi.evalSmalltalkFile fn
    ["play","expr",fn] -> stsc3_play Interpreter.Lisp.Expr.evalSmalltalkFile fn
    ["stop"] -> SC3.withSC3 SC3.reset
    _ -> putStrLn (unlines help)

