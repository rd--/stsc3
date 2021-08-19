-- | Read evaluate print loop
module Interpreter.Som.Repl where

import System.Environment {- base -}
import System.IO {- base -}
import Text.Printf {- base -}

import Interpreter.Som.Core {- stsc3 -}
import Interpreter.Som.Types {- stsc3 -}

-- | Read lines from Handle while there is input waiting.
replReadInput :: String -> Handle -> IO String
replReadInput s h = do
  l <- hGetLine h -- no eol
  r <- hReady h
  let s' = s ++ (l ++ "\n")
  if r then replReadInput s' h else return s'

{- | The read-eval-print loop continue function.
     Read program text, evaluate it, report errors or result, continue with the modified state.
-}
replContinue :: VMState -> IO ()
replContinue vmState = do
  str <- replReadInput "" stdin
  (result,vmState') <- vmEval vmState str
  case result of
    Left msg -> putStrLn ("error: " ++ msg) >> replContinue vmState
    Right res -> putStr "result: " >> objectPrint res >> replContinue vmState'

-- | Main function for read-eval-print loop.
replMain :: FilePath -> IO ()
replMain somDirectory = initialGlobalDictionary somDirectory >>= vmStateInit >>= replContinue

{- | Generate Smalltalk expression to load and run class.

> runSomClassSmalltalk "TestHarness" ["BlockTest"]
-}
runSomClassSmalltalk :: String -> [String] -> String
runSomClassSmalltalk cl arg =
  let quote x = printf "'%s'" x
  in printf "%s new run: #(%s)" cl (unwords (map quote (cl : arg)))

{- | Load and run Smalltalk class.

> loadAndRunClass "TestHarness" []
> loadAndRunClass "TestHarness" ["String"]
> loadAndRunClass "Harness" ["Bounce"]
-}
loadAndRunClass :: FilePath -> String -> [String] -> IO ()
loadAndRunClass somDirectory cl arg = do
  st <- initialGlobalDictionary somDirectory >>= vmStateInit
  (result,_) <- vmEval st (runSomClassSmalltalk cl arg)
  case result of
    Left msg -> putStrLn ("error: " ++ msg)
    Right res -> putStr "result: " >> objectPrint res >> return ()

{- | If there are no arguments start a read-evaluate-print loop.
     If there is on or more arguments,
     load the class defined at the first and call the run: method with the remainder.
-}
somReplMain :: IO ()
somReplMain = do
  let somDirectory = "/home/rohan/opt/src/som/SOM/Smalltalk/"
  a <- getArgs
  case a of
    [] -> replMain somDirectory
    cl:arg -> loadAndRunClass somDirectory cl arg
