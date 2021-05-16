import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import Data.Char {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}
import System.IO {- base -}

import qualified Data.Map as Map {- containers -}

import Sound.SC3 {- hsc3 -}

import qualified Sound.SC3.UGen.Dot {- hsc3-dot -}

import qualified Sound.SC3.Lisp.Env as Env {- hsc3-lisp -}

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
  | UGenObject UGen
  | SymbolObject String
  | ArrayObject [Object]
  | BlockObject (Env.Env Object) St.BlockBody

instance Show Object where
  show o =
    case o of
      NilObject -> "nil"
      SymbolObject x -> show x
      ArrayObject x -> unwords (map show x)
      BlockObject _ _ -> "Block"
      UGenObject x -> case x of
                        Constant_U c -> show (constantValue c)
                        _ -> show x

identifier_to_object :: St.Identifier -> Object
identifier_to_object x = SymbolObject x

literal_to_object :: St.Literal -> Object
literal_to_object l =
  case l of
    St.NumberLiteral (Left x) -> UGenObject (constant x)
    St.NumberLiteral (Right x) -> UGenObject (constant x)
    St.StringLiteral _ -> error "literal_to_object?"
    St.CharacterLiteral _ -> error "literal_to_object?"
    St.SymbolLiteral x -> SymbolObject x
    St.SelectorLiteral _ -> error "literal_to_object?"
    St.ArrayLiteral x -> ArrayObject (map (either literal_to_object identifier_to_object) x)

-- | Add temporaries to environment, initialised to nil.  In
-- evaluation this will be preceded by a capture of the current
-- environment, which will later be reinstated.
temporariesEval :: St.Temporaries -> VM ()
temporariesEval x = put =<< liftIO . Env.env_add_frame (zip x (repeat NilObject)) =<< get

-- > evalPrimary (St.PrimaryLiteral (St.NumberLiteral (Left 15)))
evalPrimary :: St.Primary -> VM Object
evalPrimary p =
  case p of
    St.PrimaryIdentifier x -> get >>= \e -> Env.env_lookup x e
    St.PrimaryLiteral x -> return (literal_to_object x)
    St.PrimaryBlock x -> get >>= \e -> return (BlockObject e x)
    St.PrimaryExpression x -> evalExpression x -- can assign
    St.PrimaryArrayExpression x -> fmap ArrayObject (mapM evalBasicExpression x)

evalExpression :: St.Expression -> VM Object
evalExpression expr =
  case expr of
    St.ExprAssignment x -> evalAssignment x
    St.ExprBasic x -> evalBasicExpression x

evalAssignment :: St.Assignment -> VM Object
evalAssignment = undefined


evalBinaryArgument :: St.BinaryArgument -> VM Object
evalBinaryArgument (p,u) =evalPrimary p >>= \o -> maybe (return o) (evalUnaryMessageSeq o) u

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

evalUnaryMessage :: Object -> St.UnaryMessage -> VM Object
evalUnaryMessage o (St.UnaryMessage m) =
  case o of
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
        "play" -> liftIO (audition x) >> return NilObject
        "draw" -> liftIO (Sound.SC3.UGen.Dot.draw x) >> return NilObject
        _ -> throwError "evalUnaryNumFunc"
    _ -> throwError "evalUnaryNumFunc"

evalUnaryMessageSeq :: Object -> [St.UnaryMessage] -> VM Object
evalUnaryMessageSeq o sq =
  case sq of
    [] -> return o
    u:sq' -> evalUnaryMessage o u >>= \r -> evalUnaryMessageSeq r sq'

messagesRewrite :: St.Messages -> Maybe St.Messages
messagesRewrite m =
  case m of
    St.MessagesUnary ([],Just b,k) -> Just (St.MessagesBinary (b,k))
    St.MessagesUnary ([],Nothing,Just k) -> Just (St.MessagesKeyword k)
    St.MessagesUnary ([],Nothing,Nothing) -> Nothing
    St.MessagesBinary ([],Just k) -> Just (St.MessagesKeyword k)
    St.MessagesBinary ([],Nothing) -> Nothing
    St.MessagesKeyword k -> Just (St.MessagesKeyword k)

evalMessagesSend :: Object -> St.Messages -> VM Object
evalMessagesSend o m =
  case m of
    St.MessagesUnary (u,b,k) -> do
      r <- evalUnaryMessageSeq o u
      case messagesRewrite (St.MessagesUnary ([],b,k)) of
        Just m' -> evalMessagesSend r m'
        Nothing -> return r
    St.MessagesBinary (b,k) -> do
      r <- evalBinaryMessageSeq o b
      case messagesRewrite (St.MessagesBinary ([],k)) of
        Just m' -> evalMessagesSend r m'
        Nothing -> return r
    St.MessagesKeyword k -> throwError "evalMessagesSend: keyword?"

evalBasicExpression :: St.BasicExpression -> VM Object
evalBasicExpression expr =
  case expr of
    (p,Nothing,Nothing) -> evalPrimary p
    (p,Just m,Nothing) -> evalPrimary p >>= \o -> evalMessagesSend o m
    _ -> error "eval_basicexpression?"

statementsEval :: St.Statements -> VM Object
statementsEval st =
  case st of
    St.StatementsReturn _ -> throwError "StatementsReturn?"
    St.StatementsExpression expr cnt -> evalExpression expr >>= \r -> maybe (return r) statementsEval cnt

initializerDefinitionEval :: St.InitializerDefinition -> VM Object
initializerDefinitionEval (St.InitializerDefinition tm st) = do
  case (tm,st) of
    (_,Nothing) -> return NilObject
    (Nothing,Just st') -> statementsEval st'
    (Just tm',Just st') -> temporariesEval tm' >> statementsEval st'

programElementEval :: St.ProgramElement -> VM Object
programElementEval el =
  case el of
    St.ProgramGlobal _ -> throwError "ProgramGlobal?"
    St.ProgramInitializer x -> initializerDefinitionEval x

evalString :: String -> VM Object
evalString txt = do
  let [st] = St.stParse St.smalltalkProgram txt
  programElementEval st

coreDict :: Env.Dict Object
coreDict = Map.fromList [("true",UGenObject (constant 1)),("false",UGenObject (constant 0))]

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
