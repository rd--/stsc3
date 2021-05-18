import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import Data.Maybe {- base -}
import System.Environment {- base -}
import System.IO {- base -}

import qualified Data.Map as Map {- containers -}

import qualified Sound.SC3 as SC3 {- hsc3 -}
import qualified Sound.SC3.UGen.Plain as Plain {- hsc3 -}

import qualified Sound.SC3.UGen.Dot {- hsc3-dot -}

import qualified Sound.SC3.Lisp.Env as Env {- hsc3-lisp -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

import qualified Language.Smalltalk.Parser as St {- stsc3 -}

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
    ]

main :: IO ()
main = do
  a <- getArgs
  case a of
    "cat":fn_seq -> mapM_ (\fn -> putStrLn fn >> st_cat_file fn) fn_seq
    _ -> putStrLn (unlines help)

-- * Interpreter

type VM t = Env.EnvMonad IO Object t

data Object
  = NilObject
  | UGenClassObject String DB.U
  | UGenObject SC3.UGen
  | SymbolObject String
  | ArrayObject [Object]
  | BlockObject (Env.Env Object) St.BlockBody

-- | Extract UGenObject, in VM in case of error
objectUGen :: Object ->  VM SC3.UGen
objectUGen o =
  case o of
    UGenObject x -> return x
    _ -> throwError "objectUGen?"

instance Show Object where
  show o =
    case o of
      NilObject -> "nil"
      SymbolObject x -> show x
      ArrayObject x -> unwords (map show x)
      BlockObject _ _ -> "Block"
      UGenClassObject x _ -> x
      UGenObject x -> case x of
                        SC3.Constant_U c -> show (SC3.constantValue c)
                        _ -> show x

identifierToObject :: St.Identifier -> Object
identifierToObject x = SymbolObject x

isUGenName :: String -> Bool
isUGenName x = x `elem` map DB.ugen_name DB.ugen_db

literalToObject :: St.Literal -> Object
literalToObject l =
  case l of
    St.NumberLiteral (Left x) -> UGenObject (SC3.constant x)
    St.NumberLiteral (Right x) -> UGenObject (SC3.constant x)
    St.StringLiteral _ -> error "literalToObject?"
    St.CharacterLiteral _ -> error "literalToObject?"
    St.SymbolLiteral x -> SymbolObject x
    St.SelectorLiteral _ -> error "literalToObject?"
    St.ArrayLiteral x -> ArrayObject (map (either literalToObject identifierToObject) x)

-- | Add temporaries to environment, initialised to nil.
evalTemporaries :: St.Temporaries -> VM ()
evalTemporaries x = put =<< liftIO . Env.env_add_frame (zip x (repeat NilObject)) =<< get

-- | Delete frame and return input value.
deleteFrame :: t -> VM t
deleteFrame r = (put =<< Env.env_del_frame =<< get) >> return r

-- > evalPrimary (St.PrimaryLiteral (St.NumberLiteral (Left 15)))
evalPrimary :: St.Primary -> VM Object
evalPrimary p =
  case p of
    St.PrimaryIdentifier x -> if isUGenName x then return (UGenClassObject x (DB.u_lookup_cs_err x)) else get >>= \e -> Env.env_lookup x e
    St.PrimaryLiteral x -> return (literalToObject x)
    St.PrimaryBlock x -> get >>= \e -> return (BlockObject e x)
    St.PrimaryExpression x -> evalExpression x -- can assign
    St.PrimaryArrayExpression x -> fmap ArrayObject (mapM evalBasicExpression x)

evalExpression :: St.Expression -> VM Object
evalExpression expr =
  case expr of
    St.ExprAssignment x -> evalAssignment x
    St.ExprBasic x -> evalBasicExpression x

evalAssignment :: St.Assignment -> VM Object
evalAssignment (St.Assignment lhs rhs) = do
  env <- get
  rhsValue <- evalExpression rhs
  liftIO (Env.env_set env lhs rhsValue)
  return NilObject

evalBinaryArgument :: St.BinaryArgument -> VM Object
evalBinaryArgument (p,u) = evalPrimary p >>= \o -> maybe (return o) (evalUnaryMessageSeq o) u

evalBinaryMessage :: Object -> St.BinaryMessage -> VM Object
evalBinaryMessage lhs (St.BinaryMessage (m,a)) = do
  rhs <- evalBinaryArgument a
  case (lhs,rhs) of
    (UGenObject x,UGenObject y) ->
      case m of
        "+" -> return (UGenObject (x + y))
        "-" -> return (UGenObject (x - y))
        "*" -> return (UGenObject (x * y))
        "/" -> return (UGenObject (x / y))
        "**" -> return (UGenObject (x ** y))
        _ -> throwError "evalBinaryMessage"
    _ -> throwError "evalBinaryMessage"

evalBinaryMessageSeq :: Object -> [St.BinaryMessage] -> VM Object
evalBinaryMessageSeq o sq =
  case sq of
    [] -> return o
    b:sq' -> evalBinaryMessage o b >>= \r -> evalBinaryMessageSeq r sq'

extendEnvironment :: Env.Env t -> [(Env.Name,t)] -> VM (Env.Env t)
extendEnvironment e x = if null x then return e else liftIO (Env.env_add_frame x e)

{- | evalBlock works by:
   1. saving the current environment;
   2. extending the stored block environment with the argument frame and making this the current environment;
   3. evaluating the block body in the current (extended block) environment and saving the result;
   4. restoring the saved environment;
   5. returning the saved result
-}
evalBlock :: Object -> Env.Env Object -> St.BlockBody -> [Object] -> VM Object
evalBlock _o blockEnvironment (St.BlockBody maybeBlockArguments blockTemporaries blockStatements) arguments = do
  let blockArguments = fromMaybe [] maybeBlockArguments
  when (length blockArguments /= length arguments) (throwError "evalBlock: wrong number of arguments?")
  extendedBlockEnvironment <- extendEnvironment blockEnvironment (zip blockArguments arguments)
  currentEnvironment <- get
  put extendedBlockEnvironment
  result <- evalTemporariesStatements blockTemporaries blockStatements
  put currentEnvironment
  return result

genUId :: VM SC3.UGenId
genUId = return (SC3.UId 0)

evalUnaryMessage :: Object -> St.UnaryMessage -> VM Object
evalUnaryMessage o (St.UnaryMessage m) =
  case o of
    BlockObject e x ->
      case m of
        "value" -> evalBlock o e x []
        _ -> throwError "evalUnaryMessage: Block?"
    UGenClassObject x u -> do
      uid <- if DB.ugen_nondet u then genUId else throwError "evalUnaryMessage: UGen: nonDet?"
      let rt = DB.ugen_default_rate u
          nc = fromMaybe (error "evalUnaryMessage: UGen: numChan?") (DB.ugen_outputs u)
      case m of
        "new" -> makeUGen x rt [] nc uid -- ie. WhiteNoise new
        _ -> throwError "evalUnaryMessage: UGenClass?"
    UGenObject x ->
      case m of
        "abs" -> return (UGenObject (abs x))
        "negate" -> return (UGenObject (negate x))
        "exp" -> return (UGenObject (exp x))
        "log" -> return (UGenObject (log x))
        "sqrt" -> return (UGenObject (sqrt x))
        "sin" -> return (UGenObject (sin x))
        "cos" -> return (UGenObject (cos x))
        "tan" -> return (UGenObject (tan x))
        "asin" -> return (UGenObject (asin x))
        "acos" -> return (UGenObject (acos x))
        "atan" -> return (UGenObject (atan x))
        "sinh" -> return (UGenObject (sinh x))
        "cosh" -> return (UGenObject (cosh x))
        "tanh" -> return (UGenObject (tanh x))
        "asinh" -> return (UGenObject (asinh x))
        "acosh" -> return (UGenObject (acosh x))
        "atanh" -> return (UGenObject (atanh x))
        "play" -> liftIO (SC3.audition x) >> return NilObject
        "draw" -> liftIO (Sound.SC3.UGen.Dot.draw x) >> return NilObject
        _ -> throwError "evalUnaryNumFunc"
    _ -> throwError "evalUnaryNumFunc"

evalUnaryMessageSeq :: Object -> [St.UnaryMessage] -> VM Object
evalUnaryMessageSeq o sq =
  case sq of
    [] -> return o
    u:sq' -> evalUnaryMessage o u >>= \r -> evalUnaryMessageSeq r sq'

evalKeywordArgument :: St.KeywordArgument -> VM Object
evalKeywordArgument (p,u,b) = do
  p' <- evalPrimary p
  u' <- maybe (return p') (evalUnaryMessageSeq p') u
  maybe (return u') (evalBinaryMessageSeq p') b

makeUGen :: String -> SC3.Rate -> [Object] -> Int -> SC3.UGenId -> VM Object
makeUGen ugenName ugenRate ugenInputObjects ugenNumChan ugenId = do
  ugenInputs <- mapM objectUGen ugenInputObjects
  let u = Plain.mk_plain ugenRate ugenName ugenInputs ugenNumChan (SC3.Special 0) ugenId
  return (UGenObject (SC3.ugen_optimise_const_operator u))

{- | Where o is a SC3.UGen class:
     check keyword names match SC3.UGen input names,
     check for mulAdd inputs,
     check for numChan input,
     derive number of output channels and rate,
     check if UGen is non-determinate and if so generate UId,
-}
evalKeywordMessage :: Object -> [(St.Identifier,St.KeywordArgument)] -> VM Object
evalKeywordMessage o k = do
  let keywordNames = map fst k
  keywordValues <- mapM (evalKeywordArgument . snd) k
  case o of
    BlockObject e x -> if all (== "value:") keywordNames then evalBlock o e x keywordValues else throwError "evalKeywordMessage: block?"
    UGenClassObject x u -> do
      uid <- if DB.ugen_nondet u then genUId else return SC3.NoId
      let rt = DB.ugen_default_rate u
          nc = fromMaybe (error "evalKeywordMessage: UGen: numChan?") (DB.ugen_outputs u)
      makeUGen x rt keywordValues nc uid -- ?
    _ -> error "evalKeywordMessage"

messagesRewrite :: St.Messages -> Maybe St.Messages
messagesRewrite m =
  case m of
    St.MessagesUnary ([],Just b,k) -> Just (St.MessagesBinary (b,k))
    St.MessagesUnary ([],Nothing,Just k) -> Just (St.MessagesKeyword k)
    St.MessagesUnary ([],Nothing,Nothing) -> Nothing
    St.MessagesBinary ([],Just k) -> Just (St.MessagesKeyword k)
    St.MessagesBinary ([],Nothing) -> Nothing
    St.MessagesKeyword k -> Just (St.MessagesKeyword k)
    _ -> error "messagesRewrite?"

evalMessages :: Object -> St.Messages -> VM Object
evalMessages o m =
  case m of
    St.MessagesUnary (u,b,k) -> do
      r <- evalUnaryMessageSeq o u
      case messagesRewrite (St.MessagesUnary ([],b,k)) of
        Just m' -> evalMessages r m'
        Nothing -> return r
    St.MessagesBinary (b,k) -> do
      r <- evalBinaryMessageSeq o b
      case messagesRewrite (St.MessagesBinary ([],k)) of
        Just m' -> evalMessages r m'
        Nothing -> return r
    St.MessagesKeyword (St.KeywordMessage k) -> evalKeywordMessage o k

evalBasicExpression :: St.BasicExpression -> VM Object
evalBasicExpression expr =
  case expr of
    (p,Nothing,Nothing) -> evalPrimary p
    (p,Just m,Nothing) -> evalPrimary p >>= \o -> evalMessages o m
    _ -> error "eval_basicexpression?"

evalStatements :: St.Statements -> VM Object
evalStatements st =
  case st of
    St.StatementsReturn _ -> throwError "StatementsReturn?"
    St.StatementsExpression expr cnt -> evalExpression expr >>= \r -> maybe (return r) evalStatements cnt

evalTemporariesStatements :: Maybe St.Temporaries -> Maybe St.Statements -> VM Object
evalTemporariesStatements tm st = do
  case (tm,st) of
    (_,Nothing) -> return NilObject
    (Nothing,Just st') -> evalStatements st'
    (Just tm',Just st') -> evalTemporaries tm' >> evalStatements st' >>= deleteFrame

evalProgramElement :: St.ProgramElement -> VM Object
evalProgramElement el =
  case el of
    St.ProgramGlobal _ -> throwError "ProgramGlobal?"
    St.ProgramInitializer (St.InitializerDefinition tm st) -> evalTemporariesStatements tm st

evalString :: String -> VM Object
evalString txt = do
  let [st] = St.stParse St.smalltalkProgram txt
  evalProgramElement st

coreDict :: Env.Dict Object
coreDict =
  Map.fromList
  [("true",UGenObject (SC3.constant 1))
  ,("false",UGenObject (SC3.constant 0))]

getProgram :: String -> Handle -> IO String
getProgram s h = do
  l <- hGetLine h -- no eol
  r <- hReady h
  let s' = s ++ (l ++ "\n")
  if r then getProgram s' h else return s'

replCont :: Env.Env Object -> IO ()
replCont env = do
  str <- getProgram "" stdin
  (r,env') <- runStateT (runExceptT (evalString str)) env
  case r of
    Left msg -> putStrLn ("error: " ++ msg) >> replCont env
    Right res -> putStrLn ("result: " ++ show res) >> replCont env'

replInit :: IO ()
replInit = do
  env <- Env.env_gen_toplevel coreDict :: IO (Env.Env Object)
  replCont env
