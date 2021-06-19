import System.Environment {- base -}

import qualified Sound.SC3 as SC3 {- hsc3 -}

import qualified Sound.SC3.UGen.Dot as Dot {- hsc3-dot -}

import qualified Language.Smalltalk.Parser as St {- stsc3 -}

import qualified Interpreter {- stsc3 -}

-- | Parse and then pretty print Smalltalk program.
st_cat :: String -> String
st_cat = St.smalltalkProgram_pp . St.stParse St.smalltalkProgram

st_cat_file :: FilePath -> IO ()
st_cat_file fn = do
  st <- readFile fn
  putStrLn (st_cat st)

help :: [String]
help =
    ["stsc3 command [arguments]"
    ," cat smalltalk-program-file..."
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
    "cat":fn_seq -> mapM_ (\fn -> putStrLn fn >> st_cat_file fn) fn_seq
    ["repl"] -> Interpreter.replInit
    ["draw",fn] -> stsc3_draw fn
    ["play",fn] -> stsc3_play fn
    ["stop"] -> SC3.withSC3 SC3.reset
    _ -> putStrLn (unlines help)

