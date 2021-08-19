-- | Simple Lisp interpter for Smalltalk expressions in the Ansi Ast.
module Interpreter.Lisp.Ansi where

import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import Data.Maybe {- base -}
import System.IO {- base -}

import qualified Sound.SC3 as SC3 {- hsc3 -}

import qualified Sound.SC3.Lisp.Env as Env {- hsc3-lisp -}

import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

import Interpreter.Lisp.Common {- stsc3 -}

type VMAnsi t = VM St.BlockBody t
type ObjectAnsi = Object St.BlockBody

evalPrimary :: St.Primary -> VMAnsi ObjectAnsi
evalPrimary p =
  case p of
    St.PrimaryIdentifier x -> lookupIdentifier x
    St.PrimaryLiteral x -> literalToObject x
    St.PrimaryBlock x -> get >>= \e -> return (BlockObject e x)
    St.PrimaryExpression x -> evalExpression x -- can assign
    St.PrimaryArrayExpression x -> arrayToObject =<< (mapM evalBasicExpression x)

evalExpression :: St.Expression -> VMAnsi ObjectAnsi
evalExpression expr =
  case expr of
    St.ExprAssignment x -> evalAssignment x
    St.ExprBasic x -> evalBasicExpression x

evalAssignment :: St.Assignment -> VMAnsi ObjectAnsi
evalAssignment (St.Assignment lhs rhs) = do
  env <- get
  rhsValue <- evalExpression rhs
  liftIO (Env.envSet env lhs rhsValue)

evalBinaryArgument :: St.BinaryArgument -> VMAnsi ObjectAnsi
evalBinaryArgument (St.BinaryArgument p u) =
  evalPrimary p >>= \o -> maybe (return o) (evalUnaryMessageSeq o) u

evalBinaryMessage :: ObjectAnsi -> St.BinaryMessage -> VMAnsi ObjectAnsi
evalBinaryMessage o (St.BinaryMessage m a) = do
  rhs <- evalBinaryArgument a
  evalBinaryUGenMessage o m rhs

evalBinaryMessageSeq :: ObjectAnsi -> [St.BinaryMessage] -> VMAnsi ObjectAnsi
evalBinaryMessageSeq o sq =
  case sq of
    [] -> return o
    b:sq' -> evalBinaryMessage o b >>= \r -> evalBinaryMessageSeq r sq'

{- | evalBlock works by:
   1. saving the current environment;
   2. extending the stored block environment with the argument frame and making this the current environment;
   3. evaluating the block body in the current (extended block) environment and saving the result;
   4. restoring the saved environment;
   5. returning the saved result
-}
evalBlock :: Env.Env Name ObjectAnsi -> St.BlockBody -> [ObjectAnsi] -> VMAnsi ObjectAnsi
evalBlock blockEnvironment (St.BlockBody _ maybeBlockArguments blockTemporaries blockStatements) arguments = do
  let blockArguments = fromMaybe [] maybeBlockArguments
  when (length blockArguments /= length arguments) (throwError "evalBlock: wrong number of arguments?")
  extendedBlockEnvironment <- extendEnvironment blockEnvironment (zip blockArguments arguments)
  currentEnvironment <- get
  put extendedBlockEnvironment
  result <- evalTemporariesStatements blockTemporaries blockStatements
  put currentEnvironment
  return result

evalUnaryMessage :: ObjectAnsi -> St.UnaryMessage -> VMAnsi ObjectAnsi
evalUnaryMessage o (St.UnaryMessage m) =
  case o of
    BlockObject e x ->
      case m of
        "value" -> evalBlock e x []
        _ -> throwError ("evalUnaryMessage: BlockObject: " ++ m)
    ClassObject c ->
      case (c,m) of
        ("SC3","reset") -> liftIO (SC3.withSC3 SC3.reset) >> return NilObject
        ("Float","pi") -> return (UGenObject (SC3.double_to_ugen pi))
        _ -> throwError ("evalUnaryMessage: ClassObject: " ++ c)
    UGenClassObject x u -> do
      uid <- if DB.ugen_nondet u then genUId else return SC3.NoId
      let rt = DB.ugen_default_rate u
          nc = fromMaybe (error "evalUnaryMessage: UGen: numChan?") (DB.ugen_outputs u)
      case m of
        "new" -> makeUGen x rt [] nc uid [] (DB.ugen_std_mce u > 0) -- ie. WhiteNoise new
        _ -> throwError "evalUnaryMessage: UGenClassObject?"
    UGenObject _ -> evalUnaryUGenMessage o m
    _ -> throwError ("evalUnaryMessage: Object: " ++ m)

evalUnaryMessageSeq :: ObjectAnsi -> [St.UnaryMessage] -> VMAnsi ObjectAnsi
evalUnaryMessageSeq o sq =
  case sq of
    [] -> return o
    u:sq' -> evalUnaryMessage o u >>= \r -> evalUnaryMessageSeq r sq'

evalKeywordArgument :: St.KeywordArgument -> VMAnsi ObjectAnsi
evalKeywordArgument (St.KeywordArgument p u b) = do
  primary <- evalPrimary p
  unary <- maybe (return primary) (evalUnaryMessageSeq primary) u
  maybe (return unary) (evalBinaryMessageSeq unary) b

evalKeywordMessage :: ObjectAnsi -> [(St.Identifier,St.KeywordArgument)] -> VMAnsi ObjectAnsi
evalKeywordMessage o k = do
  keywordValues <- mapM (evalKeywordArgument . snd) k
  let keywordNames = map fst k
      keywordArguments = zip keywordNames keywordValues
  case o of
    BlockObject e x -> evalBlock e x keywordValues
    UGenClassObject x u -> evalKeywordUGenClassMessage x u keywordArguments
    UGenObject _ -> evalKeywordUGenMessage evalBlock o keywordArguments
    ClassObject x -> evalKeywordClassMessage evalBlock x keywordArguments
    _ -> throwError "evalKeywordMessage"

messagesRewrite :: St.Messages -> Maybe St.Messages
messagesRewrite m =
  case m of
    St.MessagesUnary [] (Just b) k -> Just (St.MessagesBinary b k)
    St.MessagesUnary [] Nothing (Just k) -> Just (St.MessagesKeyword k)
    St.MessagesUnary [] Nothing Nothing -> Nothing
    St.MessagesBinary [] (Just k) -> Just (St.MessagesKeyword k)
    St.MessagesBinary [] Nothing -> Nothing
    St.MessagesKeyword k -> Just (St.MessagesKeyword k)
    _ -> error "messagesRewrite?"

evalMessages :: ObjectAnsi -> St.Messages -> VMAnsi ObjectAnsi
evalMessages o m =
  case m of
    St.MessagesUnary u b k -> do
      r <- evalUnaryMessageSeq o u
      case messagesRewrite (St.MessagesUnary [] b k) of
        Just m' -> evalMessages r m'
        Nothing -> return r
    St.MessagesBinary b k -> do
      r <- evalBinaryMessageSeq o b
      case messagesRewrite (St.MessagesBinary [] k) of
        Just m' -> evalMessages r m'
        Nothing -> return r
    St.MessagesKeyword (St.KeywordMessage k) -> evalKeywordMessage o k

evalBasicExpression :: St.BasicExpression -> VMAnsi ObjectAnsi
evalBasicExpression expr =
  case expr of
    St.BasicExpression p Nothing Nothing -> evalPrimary p
    St.BasicExpression p (Just m) Nothing -> evalPrimary p >>= \o -> evalMessages o m
    _ -> throwError "eval_basicexpression?"

evalStatements :: St.Statements -> VMAnsi ObjectAnsi
evalStatements st =
  case st of
    St.StatementsReturn _ -> throwError "StatementsReturn?"
    St.StatementsExpression expr cnt -> evalExpression expr >>= \r -> maybe (return r) evalStatements cnt

evalTemporariesStatements :: Maybe St.Temporaries -> Maybe St.Statements -> VMAnsi ObjectAnsi
evalTemporariesStatements tm st = do
  case (tm,st) of
    (_,Nothing) -> return NilObject
    (Nothing,Just st') -> evalStatements st'
    (Just tm',Just st') -> evalTemporaries tm' >> evalStatements st' >>= deleteFrame

evalProgramElement :: St.ProgramElement -> VMAnsi ObjectAnsi
evalProgramElement el =
  case el of
    St.ProgramGlobal _ -> throwError "ProgramGlobal?"
    St.ProgramInitializer (St.InitializerDefinition tm st) -> evalTemporariesStatements tm st

evalString :: String -> VMAnsi ObjectAnsi
evalString txt = do
  let St.SmalltalkProgram [st] = St.stParse St.smalltalkProgram txt
  evalProgramElement st

replCont :: Env.Env Name ObjectAnsi -> IO ()
replCont env = do
  str <- getProgram "" stdin
  (r,env') <- runStateT (runExceptT (evalString str)) env
  case r of
    Left msg -> putStrLn ("error: " ++ msg) >> replCont env
    Right res -> putStrLn ("result: " ++ show res) >> replCont env'

replMain :: IO ()
replMain = do
  env <- initialEnvironment
  replCont env

-- > evalSmalltalkFile "/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st"
evalSmalltalkFile :: FilePath -> IO SC3.UGen
evalSmalltalkFile fn = do
  str <- readFile fn
  env <- initialEnvironment
  (r,_) <- runStateT (runExceptT (evalString str)) env
  case r of
    Right (UGenObject res) -> return res
    Right _ -> error "evalSmalltalkFile: error: not UGen?"
    Left msg -> error ("evalSmalltalkFile: error: " ++ msg)
