import System.Environment {- base -}

import qualified Sound.SC3 as SC3 {- hsc3 -}

import qualified Sound.SC3.UGen.Dot as Dot {- hsc3-dot -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Lexer as St.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Parser as St.Parser {- stsc3 -}

import qualified Interpreter {- stsc3 -}

-- | Parse and then pretty print Smalltalk program.
st_cat_parsec :: String -> String
st_cat_parsec = St.smalltalkProgram_pp . St.stParse St.smalltalkProgram

st_cat_happy :: String -> String
st_cat_happy = St.smalltalkProgram_pp . St.Parser.smalltalkParser . St.Lexer.alexScanTokens

st_cat_file :: String -> FilePath -> IO ()
st_cat_file which fn = do
  st <- readFile fn
  putStrLn ((if which == "parsec" then st_cat_parsec else st_cat_happy) st)

help :: [String]
help =
    ["stsc3 command [arguments]"
    ," cat {parsec|happy} smalltalk-program-file..."
    ," repl"
    ]

stsc3_play :: FilePath -> IO ()
stsc3_play fn = Interpreter.evalSmalltalkFile fn >>= SC3.audition

stsc3_draw :: FilePath -> IO ()
stsc3_draw fn = Interpreter.evalSmalltalkFile fn >>= Dot.draw . SC3.out 0

main :: IO ()
main = do
  a <- getArgs
  case a of
    "cat":which:fn_seq -> mapM_ (\fn -> putStrLn fn >> st_cat_file which fn) fn_seq
    ["repl"] -> Interpreter.replInit
    ["draw",fn] -> stsc3_draw fn
    ["play",fn] -> stsc3_play fn
    ["stop"] -> SC3.withSC3 SC3.reset
    _ -> putStrLn (unlines help)

