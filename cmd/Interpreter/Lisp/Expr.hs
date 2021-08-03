-- | Simple Lisp interpter for Smalltalk expressions in the Expr Ast.
module Interpreter.Lisp.Expr where

import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import Data.Maybe {- base -}
import System.IO {- base -}

import qualified Sound.SC3 as SC3 {- hsc3 -}

import qualified Sound.SC3.Lisp.Env as Env {- hsc3-lisp -}

import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr as Expr {- stsc3 -}

import Interpreter.Lisp.Common {- stsc3 -}

type Proc = ([St.BlockArgument],St.Temporaries,[Expr.Expr])
type VMExpr t = VM Proc t
type ObjectExpr = Object Proc

evalExpr :: Expr.Expr -> VMExpr ObjectExpr
evalExpr expr =
  case expr of
    Expr.Identifier x -> lookupIdentifier x
    Expr.Literal x -> literalToObject x
    Expr.Assignment lhs rhs -> evalAssignment lhs rhs
    Expr.Return _ -> throwError "Return"
    Expr.Send e m -> evalSend e m
    Expr.Lambda arg tmp stm -> get >>= \e -> return (BlockObject e (arg,tmp,stm))
    Expr.Array x -> arrayToObject =<< (mapM evalExpr x)
    Expr.Begin x -> fmap last (mapM evalExpr x)
    Expr.Init tmp stm -> evalTemporariesStatements tmp stm

evalAssignment :: St.Identifier -> Expr.Expr -> VMExpr ObjectExpr
evalAssignment lhs rhs = do
  env <- get
  rhsValue <- evalExpr rhs
  liftIO (Env.env_set env lhs rhsValue)
  return NilObject

evalSend :: Expr.Expr -> Expr.Message -> VMExpr ObjectExpr
evalSend rcv (Expr.Message msg arg) = do
  p <- evalExpr rcv
  keywordValues <- mapM evalExpr arg
  case (msg,keywordValues) of
    (St.UnarySelector sel,[]) -> evalUnaryMessage p sel
    (St.BinarySelector sel,[p1]) -> evalBinaryUGenMessage p sel p1
    (St.KeywordSelector sel,_) ->
      let keywordArguments = zip (St.keywordSelectorElements sel) keywordValues
      in case p of
           BlockObject e x -> evalBlock e x keywordValues
           UGenClassObject x u -> evalKeywordUGenClassMessage x u keywordArguments
           UGenObject _ -> evalKeywordUGenMessage evalBlock p keywordArguments
           ClassObject x -> evalKeywordClassMessage evalBlock x keywordArguments
           _ -> throwError "evalKeywordMessage"
    _ -> throwError "evalSend"

evalUnaryMessage :: ObjectExpr -> St.Identifier -> VMExpr ObjectExpr
evalUnaryMessage o m =
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

{- | evalBlock works by:
   1. saving the current environment;
   2. extending the stored block environment with the argument frame and making this the current environment;
   3. evaluating the block body in the current (extended block) environment and saving the result;
   4. restoring the saved environment;
   5. returning the saved result
-}
evalBlock :: Env.Env () ObjectExpr -> Proc -> [ObjectExpr] -> VMExpr ObjectExpr
evalBlock blockEnvironment (blockArguments, blockTemporaries, blockStatements) arguments = do
  when (length blockArguments /= length arguments) (throwError "evalBlock: wrong number of arguments?")
  extendedBlockEnvironment <- extendEnvironment blockEnvironment (zip blockArguments arguments)
  currentEnvironment <- get
  put extendedBlockEnvironment
  result <- evalTemporariesStatements blockTemporaries blockStatements
  put currentEnvironment
  return result

evalTemporariesStatements :: St.Temporaries -> [Expr.Expr] -> VMExpr ObjectExpr
evalTemporariesStatements tm st = do
  case (St.temporariesLength tm,st) of
    (_,[]) -> return NilObject
    (0,_) -> fmap last (mapM evalExpr st)
    _ -> evalTemporaries tm >> fmap last (mapM evalExpr st) >>= deleteFrame

evalString :: String -> VMExpr ObjectExpr
evalString txt = evalExpr (Expr.smalltalkProgramExpr (St.stParse St.smalltalkProgram txt))

replCont :: Env.Env () ObjectExpr -> IO ()
replCont env = do
  str <- getProgram "" stdin
  (r,env') <- runStateT (runExceptT (evalString str)) env
  case r of
    Left msg -> putStrLn ("error: " ++ msg) >> replCont env
    Right res -> putStrLn ("result: " ++ show res) >> replCont env'

replInit :: IO ()
replInit = do
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
