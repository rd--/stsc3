module Interpreter where

import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import Data.Bits {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.IO {- base -}

import qualified Data.Map as Map {- containers -}

import qualified Sound.SC3 as SC3 {- hsc3 -}
import qualified Sound.SC3.UGen.Plain as Plain {- hsc3 -}

import qualified Sound.SC3.Lang.Random.IO as Random {- hsc3-lang -}

import qualified Sound.SC3.UGen.Dot {- hsc3-dot -}

import qualified Sound.SC3.Lisp.Env as Env {- hsc3-lisp -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Bindings.Smalltalk as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

import qualified Language.Smalltalk.Parser as St {- stsc3 -}

-- | VM is simply an environment.
type VM t = Env.EnvMonad IO Object t

-- | Object
data Object
  = NilObject
  | ClassObject String
  | UGenClassObject String DB.U
  | UGenObject SC3.UGen
  | SymbolObject String -- ^ There is no separate string type.
  | ArrayObject [Object]
  | BlockObject (Env.Env Object) St.BlockBody

instance Show Object where
  show o =
    case o of
      NilObject -> "nil"
      SymbolObject x -> show x
      ArrayObject x -> "{" ++ intercalate ". " (map show x) ++ "}"
      BlockObject _ _ -> "Block * *"
      UGenClassObject x _ -> x
      UGenObject x -> case x of
                        SC3.Constant_U c -> show (SC3.constantValue c)
                        _ -> show x
      ClassObject x -> x

-- | Object to UGen
objectToUGen :: Object ->  VM SC3.UGen
objectToUGen o =
  case o of
    UGenObject x -> return x
    ArrayObject x -> fmap SC3.mce (mapM objectToUGen x)
    _ -> throwError "objectToUGen: Object not UGen?"

-- | Object to Int
objectToInt :: String -> Object ->  VM Int
objectToInt msg o =
   case o of
    UGenObject x -> maybe (throwError ("objectToInt: " ++ msg)) (return . round . SC3.constantValue) (SC3.un_constant x)
    _ -> throwError ("objectToInt: Object not UGen: " ++ msg)

-- | Object to Symbol
objectToSymbol :: Object ->  VM String
objectToSymbol o =
  case o of
    SymbolObject x -> return x
    _ -> throwError "objectToSymbol: Object not symbol"

-- | Object to Block
objectToBlock :: Object ->  VM (Env.Env Object,St.BlockBody)
objectToBlock o =
  case o of
    BlockObject e x -> return (e,x)
    _ -> throwError "objectToBlock: Object not Block?"

-- | Object to Double
objectToDouble :: Object ->  VM Double
objectToDouble o =
  case o of
    UGenObject x -> maybe (throwError "objectToDouble") (return . SC3.constantValue) (SC3.un_constant x)
    _ -> throwError "objectToDouble: Object not UGen?"

-- | Object to list of Object
objectToArray :: String -> Object -> VM [Object]
objectToArray msg o =
  case o of
    ArrayObject x -> return x
    _ -> throwError ("objectToArray: Object not Array: " ++ msg)

-- | Identifier (String) to Object
identifierToObject :: St.Identifier -> VM Object
identifierToObject x = return (SymbolObject x)

-- | Double to Object
doubleToObject :: Double -> Object
doubleToObject x = UGenObject (SC3.double_to_ugen x)

-- | Int to Object
intToObject :: Int -> Object
intToObject x = UGenObject (SC3.int_to_ugen x)

literalToObject :: St.Literal -> VM Object
literalToObject l =
  case l of
    St.NumberLiteral (Left x) -> return (UGenObject (SC3.constant x))
    St.NumberLiteral (Right x) -> return (UGenObject (SC3.constant x))
    St.StringLiteral x -> return (SymbolObject x) -- throwError "literalToObject: string?" -- FIXME
    St.CharacterLiteral _ -> throwError "literalToObject: character?"
    St.SymbolLiteral x -> return (SymbolObject x)
    St.SelectorLiteral (St.UnarySelector "dinf") -> return (UGenObject SC3.dinf)
    St.SelectorLiteral _ -> throwError "literalToObject: selector?"
    St.ArrayLiteral x -> fmap ArrayObject (mapM (either literalToObject identifierToObject) x)

-- | Add temporaries as single frame to environment, initialised to nil.
evalTemporaries :: St.Temporaries -> VM ()
evalTemporaries x = put =<< liftIO . Env.env_add_frame (zip x (repeat NilObject)) =<< get

-- | Delete frame and return input value.
deleteFrame :: t -> VM t
deleteFrame r = (put =<< Env.env_del_frame =<< get) >> return r

isCapitalised :: String -> Bool
isCapitalised x =
  case x of
    c:_ -> isUpper c
    _ -> False

isUGenName :: String -> Bool
isUGenName x = x `elem` map DB.ugen_name DB.ugen_db

-- | Lookup identifier, which is either a UGen Class name, or a Class name, or a value in the environment.
lookupIdentifier :: String -> VM Object
lookupIdentifier x =
  if isUGenName x
  then return (UGenClassObject x (DB.u_lookup_cs_err x))
  else if isCapitalised x
       then return (ClassObject x)
       else get >>= \e -> Env.env_lookup x e

evalPrimary :: St.Primary -> VM Object
evalPrimary p =
  case p of
    St.PrimaryIdentifier x -> lookupIdentifier x
    St.PrimaryLiteral x -> literalToObject x
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

asUGenInput :: Object -> VM Object
asUGenInput o =
  case o of
    ArrayObject x -> fmap (UGenObject . SC3.mce) (mapM (\i -> asUGenInput i >>= objectToUGen) x)
    _ -> return o

evalBinaryMessage :: Object -> St.BinaryMessage -> VM Object
evalBinaryMessage o (St.BinaryMessage (m,a)) = do
  lhs <- asUGenInput o
  rhs <- asUGenInput =<< evalBinaryArgument a
  case (lhs,rhs) of
    (UGenObject x,UGenObject y) ->
      case m of
        "+" -> return (UGenObject (x + y))
        "-" -> return (UGenObject (x - y))
        "*" -> return (UGenObject (x * y))
        "/" -> return (UGenObject (x / y))
        "%" -> return (UGenObject (x `SC3.modE` y))
        ">" -> return (UGenObject (x `SC3.greater_than` y))
        ">=" -> return (UGenObject (x `SC3.greater_than_or_equal_to` y))
        "<" -> return (UGenObject (x `SC3.less_than` y))
        "<=" -> return (UGenObject (x `SC3.less_than_or_equal_to` y))
        "==" -> return (UGenObject (x `SC3.equal_to` y))
        _ -> throwError ("evalBinaryMessage: UGen: " ++ m)
    _ -> throwError ("evalBinaryMessage: " ++ show (m,lhs,rhs))

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
evalBlock :: Env.Env Object -> St.BlockBody -> [Object] -> VM Object
evalBlock blockEnvironment (St.BlockBody maybeBlockArguments blockTemporaries blockStatements) arguments = do
  let blockArguments = fromMaybe [] maybeBlockArguments
  when (length blockArguments /= length arguments) (throwError "evalBlock: wrong number of arguments?")
  extendedBlockEnvironment <- extendEnvironment blockEnvironment (zip blockArguments arguments)
  currentEnvironment <- get
  put extendedBlockEnvironment
  result <- evalTemporariesStatements blockTemporaries blockStatements
  put currentEnvironment
  return result

evalBlockError :: Env.Env Object -> St.BlockBody -> [St.Identifier] -> [Object] -> VM Object
evalBlockError e x keywordNames keywordValues =
  if all (== "value:") keywordNames
  then evalBlock e x keywordValues
  else throwError "evalBlock?"

genUId :: VM SC3.UGenId
genUId = do
  x <- liftIO SC3.generateUId
  return (SC3.UId x)

genRand :: Object -> VM Object
genRand o = do
  x <- objectToDouble o
  fmap doubleToObject (liftIO (Random.rand x))

genRand2 :: Object -> VM Object
genRand2 o = do
  x <- objectToDouble o
  fmap doubleToObject (liftIO (Random.rand2 x))

genRRand :: Object -> Object -> VM Object
genRRand o p1 = do
  x <- objectToDouble o
  y <- objectToDouble p1
  fmap doubleToObject (liftIO (Random.rrand x y))

genExpRand :: Object -> Object -> VM Object
genExpRand o p1 = do
  x <- objectToDouble o
  y <- objectToDouble p1
  fmap doubleToObject (liftIO (Random.exprand x y))

arrayAsLocalBuf :: [Object] -> VM Object
arrayAsLocalBuf a = do
  uid <- liftIO SC3.generateUId
  u <- mapM objectToUGen a
  return (UGenObject (SC3.asLocalBuf uid u))

arrayTranspose :: [Object] -> VM Object
arrayTranspose a = do
  m <- mapM (objectToArray "arrayTranspose") a
  return (ArrayObject (map ArrayObject (transpose m)))

arrayConcatenation :: [Object] -> VM Object
arrayConcatenation a = do
  m <- mapM (objectToArray "arrayConcatenation") a
  return (ArrayObject (concat m))

evalUnaryUGenMessage :: Object -> String -> VM Object
evalUnaryUGenMessage o m = do
  x <- objectToUGen o
  case m of
    "abs" -> liftUGen abs o
    "acos" -> liftUGen acos o
    "acosh" -> liftUGen acosh o
    "ar" -> liftUGen (SC3.rewriteToRate SC3.AR) o
    "asFloat" -> return o
    "asin" -> liftUGen asin o
    "asinh" -> liftUGen asinh o
    "atan" -> liftUGen atan o
    "atanh" -> liftUGen atanh o
    "constant" -> if SC3.isConstant x then return o else throwError "evalUnaryUGenMessage: constant?"
    "cos" -> liftUGen cos o
    "cosh" -> liftUGen cosh o
    "cubed" -> liftUGen SC3.cubed o
    "distort" -> liftUGen SC3.distort o
    "dr" -> liftUGen (SC3.rewriteToRate SC3.DR) o
    "draw" -> liftIO (Sound.SC3.UGen.Dot.draw x) >> return NilObject
    "exp" -> liftUGen exp o
    "floor" -> liftUGen SC3.floorE o
    "frac" -> liftUGen SC3.frac o
    "ir" -> liftUGen (SC3.rewriteToRate SC3.IR) o
    "kr" -> liftUGen (SC3.rewriteToRate SC3.KR) o
    "log" -> liftUGen log o
    "midicps" -> liftUGen SC3.midiCPS o
    "mix" -> liftUGen SC3.mix o
    "negated" -> liftUGen negate o
    "play" -> liftIO (SC3.audition x) >> return NilObject
    "rand" -> genRand o
    "rand2" -> genRand2 o
    "reciprocal" -> liftUGen recip o
    "round" -> liftUGen SC3.roundE o
    "sin" -> liftUGen sin o
    "sinh" -> liftUGen sinh o
    "sqrt" -> liftUGen sqrt o
    "tan" -> liftUGen tan o
    "tanh" -> liftUGen tanh o
    "transpose" -> liftUGen SC3.mceTranspose o
    _ -> throwError ("evalUnaryUGenMessage: " ++ m)

evalUnaryMessage :: Object -> St.UnaryMessage -> VM Object
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
    ArrayObject x ->
      case m of
        "asArray" -> return o
        "asLocalBuf" -> arrayAsLocalBuf x
        "atRandom" -> Random.choose x
        "transpose" -> arrayTranspose x
        "concatenation" -> arrayConcatenation x
        "size" -> return (intToObject (length x))
        "mce" -> do
          ugenArray <- mapM objectToUGen x
          return (UGenObject (SC3.mce ugenArray))
        _ -> throwError ("evalUnaryMessage: Array: " ++ m)
    _ -> throwError ("evalUnaryMessage: Object: " ++ m)

evalUnaryMessageSeq :: Object -> [St.UnaryMessage] -> VM Object
evalUnaryMessageSeq o sq =
  case sq of
    [] -> return o
    u:sq' -> evalUnaryMessage o u >>= \r -> evalUnaryMessageSeq r sq'

evalKeywordArgument :: St.KeywordArgument -> VM Object
evalKeywordArgument (p,u,b) = do
  primary <- evalPrimary p
  unary <- maybe (return primary) (evalUnaryMessageSeq primary) u
  maybe (return unary) (evalBinaryMessageSeq unary) b

makeUGen :: String -> SC3.Rate -> [Object] -> Int -> SC3.UGenId -> [Object] -> Bool -> VM Object
makeUGen ugenName ugenRate ugenInputObjects ugenNumChan ugenId optInputObjects mceInputs = do
  ugenInputs <- mapM objectToUGen ugenInputObjects
  optInputs <- mapM objectToUGen optInputObjects
  let plainInputs = (if mceInputs then SC3.halt_mce_transform else id) ugenInputs
      u = Plain.mk_plain ugenRate ugenName plainInputs ugenNumChan (SC3.Special 0) ugenId
      o = SC3.ugen_optimise_const_operator u
  m <- case optInputs of
         [] -> return o
         [mul,add] ->  return (SC3.mulAdd o mul add)
         [mul] -> return (o * mul)
         _ -> throwError "makeUGen: optInputs?"
  return (UGenObject (SC3.ugen_optimise_const_operator m))

ugenRequiredKeywordNames :: DB.U -> [String]
ugenRequiredKeywordNames = map (\x -> x ++ ":") . DB.st_gen_required_arg

intArrayFill :: Object -> Object -> VM Object
intArrayFill k blockObject = do
  i <- objectToInt "intArrayFill" k
  (e,b) <- objectToBlock blockObject
  let j = map intToObject [1 .. i]
  a <- mapM (\x -> evalBlockError e b ["value:"] [x]) j
  return (ArrayObject a)

intReplicate :: Object -> Object -> VM Object
intReplicate p1 p2 = do
  i1 <- objectToInt "intReplicate" p1
  return (ArrayObject (replicate i1 p2))

intUGenFill :: Bool -> (SC3.UGen -> SC3.UGen) -> Object -> Object -> VM Object
intUGenFill zeroIndexed f k blockObject = do
  i <- objectToInt "intUGenFill" k
  (e,b) <- objectToBlock blockObject
  let j = map intToObject (if zeroIndexed then [0 .. i - 1] else [1 .. i])
  a <- mapM (\x -> evalBlockError e b ["value:"] [x]) j
  u <- mapM objectToUGen a
  return (UGenObject (f (SC3.mce u)))

arrayDo :: [Object] -> Object -> VM Object
arrayDo x aBlock = do
  case aBlock of
    BlockObject env blockBody -> mapM_ (evalBlock env blockBody . return) x >> return NilObject
    _ -> throwError "arrayDo?"

arrayCollect :: [Object] -> Object -> VM Object
arrayCollect x aBlock = do
  case aBlock of
    BlockObject env blockBody -> fmap ArrayObject (mapM (evalBlock env blockBody . return) x)
    _ -> throwError "arrayCollect?"

arrayInjectInto :: [Object] -> Object -> Object -> VM Object
arrayInjectInto x aValue aBlock = do
  case aBlock of
    BlockObject env blockBody -> foldM (\i j -> evalBlock env blockBody [i,j]) aValue x
    _ -> throwError "arrayInjectInto?"

mceCollect :: Object -> Object -> VM Object
mceCollect o aBlock = do
  x <- objectToUGen o
  case aBlock of
    BlockObject env blockBody -> fmap ArrayObject (mapM (evalBlock env blockBody . return . UGenObject) (SC3.mceChannels x))
    _ -> throwError "mceCollect?"

intToDo :: Object -> Object -> Object -> VM Object
intToDo p1 p2 aBlock = do
  i1 <- objectToInt "intToDo" p1
  i2 <- objectToInt "intToDo" p2
  let a = map intToObject [i1 .. i2]
  case aBlock of
    BlockObject env blockBody -> mapM_ (evalBlock env blockBody . return) a >> return NilObject
    _ -> throwError "intToDo?"

liftUGen :: (SC3.UGen -> SC3.UGen) -> Object -> VM Object
liftUGen f p1 = objectToUGen p1 >>= return . UGenObject . f

liftUGen2 :: (SC3.UGen -> SC3.UGen -> SC3.UGen) -> Object -> Object -> VM Object
liftUGen2 f p1 p2 = do
  u1 <- objectToUGen p1
  u2 <- objectToUGen p2
  return (UGenObject (f u1 u2))

liftUGen3 :: (SC3.UGen -> SC3.UGen -> SC3.UGen -> SC3.UGen) -> Object -> Object -> Object -> VM Object
liftUGen3 f p1 p2 p3 = do
  u1 <- objectToUGen p1
  u2 <- objectToUGen p2
  u3 <- objectToUGen p3
  return (UGenObject (f u1 u2 u3))

intervalTo :: Object -> Object -> VM Object
intervalTo p q = do
  i <- objectToInt "intervalTo" p
  j <- objectToInt "intervalTo" q
  return (ArrayObject (map intToObject [i .. j]))

arrayFromTo :: Object -> Object -> VM Object
arrayFromTo p1 p2 = do
  i <- objectToInt "arrayFromTo" p1
  j <- objectToInt "arrayFromTo" p2
  return (ArrayObject (map intToObject [i .. j]))

arrayFromToBy :: Object -> Object -> Object -> VM Object
arrayFromToBy p1 p2 p3 = do
  i <- objectToInt "arrayFromToBy" p1
  j <- objectToInt "arrayFromToBy" p2
  k <- objectToInt "arrayFromToBy" p3
  return (ArrayObject (map intToObject [i,i + k .. j]))

ugenIfTrueIfFalse :: Object -> Object -> Object -> VM Object
ugenIfTrueIfFalse p1 p2 p3 = do
  aBool <- objectToInt "ugenIfTrueIfFalse" p1
  (e1,b1) <- objectToBlock p2
  (e2,b2) <- objectToBlock p3
  if aBool /= 0 then evalBlock e1 b1 [] else evalBlock e2 b2 []

controlInput :: Object -> Object -> VM Object
controlInput p1 p2 = do
  nm <- objectToSymbol p1
  df <- objectToDouble p2
  return (UGenObject (SC3.control SC3.KR nm df))

arrayAt :: Object -> Object -> VM Object
arrayAt o p1 = do
  a <- objectToArray "arrayAt" o
  i <- objectToInt "arrayAt" p1
  return (a !! (i - 1))

mceAt :: Object -> Object -> VM Object
mceAt o p1 = do
  u <- objectToUGen o
  i <- objectToInt "mceAt" p1
  return (UGenObject (SC3.mceChannel (i - 1) u))

evalKeywordUGenMessage :: Object -> [(String,Object)] -> VM Object
evalKeywordUGenMessage o keywordArguments =
  case keywordArguments of
    [("arrayFill:",p1)] -> intArrayFill o p1
    [("at:",p1)] -> mceAt o p1
    [("bitAnd:",p1)] -> liftUGen2 (.&.) o p1
    [("bitOr:",p1)] -> liftUGen2 (.|.) o p1
    [("bitShiftRight:",p1)] -> liftUGen2 SC3.shiftRight o p1
    [("clip2:",p1)] -> liftUGen2 SC3.clip2 o p1
    [("collect:",p1)] -> mceCollect o p1
    [("exprand:",p1)] -> genExpRand o p1
    [("gcd:",p1)] -> liftUGen2 SC3.gcdE o p1
    [("ifTrue:",p1),("ifFalse:",p2)] -> ugenIfTrueIfFalse o p1 p2
    [("inExpRangeFrom:",p1),("to:",p2)] -> liftUGen3 (\u1 u2 u3 -> SC3.linExp u1 (-1) 1 u2 u3) o p1 p2
    [("inRangeFrom:",p1),("to:",p2)] -> liftUGen3 (\u1 u2 u3 -> SC3.linLin u1 (-1) 1 u2 u3) o p1 p2
    [("lcm:",p1)] -> liftUGen2 SC3.lcmE o p1
    [("max:",p1)] -> liftUGen2 max o p1
    [("mceFill:",p1)] -> intUGenFill False id o p1
    [("mixFill:",p1)] -> intUGenFill False SC3.mix o p1
    [("mixFillZeroIndexed:",p1)] -> intUGenFill True SC3.mix o p1
    [("mul:",p1),("add:",p2)] -> liftUGen3 SC3.mulAdd o p1 p2
    [("raisedTo:",p1)] -> liftUGen2 (**) o p1
    [("rand:",p1)] -> genRRand o p1
    [("replicate:",p1)] -> intReplicate o p1
    [("round:",p1)] -> liftUGen2 SC3.roundTo o p1
    [("to:",p1),("do:",p2)] -> intToDo o p1 p2
    [("to:",p1)] -> intervalTo o p1
    _ -> throwError ("evalKeywordMessage: UGen: " ++ show (map fst keywordArguments))

{- | Where o is a SC3.UGen class:
     check keyword names match SC3.UGen input names,
     check for mulAdd inputs,
     check for numChan input,
     derive number of output channels and rate,
     check if UGen is non-determinate and if so generate UId,
-}
evalKeywordUGenClassMessage :: String -> DB.U -> [(String,Object)] -> VM Object
evalKeywordUGenClassMessage x u keywordArguments = do
  let (keywordNames,keywordValues) = unzip keywordArguments
      requiredKeywords = ugenRequiredKeywordNames u
      numRequiredKeywords = length requiredKeywords
      optKeywords = drop numRequiredKeywords keywordNames
      (requiredValues,optValues) = splitAt numRequiredKeywords keywordValues
  when (not (keywordNames `isPrefixOf` requiredKeywords) && not (optKeywords `isPrefixOf` ["mul:","add:"]))
       (throwError "evalKeywordMessage: incorrect keyword message?")
  uid <- if DB.ugen_nondet u then genUId else return SC3.NoId
  let rt = DB.ugen_default_rate u
  (nc,inputValues) <- if x == "Demand"
                      then fmap (\i -> (i,requiredValues)) (fmap (length . SC3.mceChannels) (objectToUGen (last requiredValues)))
                      else case DB.ugen_outputs u of
                             Just fixedNumChan -> return (fixedNumChan,requiredValues)
                             Nothing -> fmap (\varNumChan -> (varNumChan,tail requiredValues)) (objectToInt "numChan" (requiredValues !! 0))
  ugen <- makeUGen x rt inputValues nc uid optValues (DB.ugen_std_mce u > 0)
  return ugen

evalKeywordMessage :: Object -> [(St.Identifier,St.KeywordArgument)] -> VM Object
evalKeywordMessage o k = do
  keywordValues <- mapM (evalKeywordArgument . snd) k
  let keywordNames = map fst k
      keywordArguments = zip keywordNames keywordValues
  case o of
    BlockObject e x -> evalBlockError e x keywordNames keywordValues
    UGenClassObject x u -> evalKeywordUGenClassMessage x u keywordArguments
    UGenObject _ -> evalKeywordUGenMessage o keywordArguments
    ArrayObject x ->
      case keywordArguments of
        [("at:",p1)] -> arrayAt o p1
        [("collect:",p1)] -> arrayCollect x p1
        [("inject:",p1),("into:",p2)] -> arrayInjectInto x p1 p2
        [("do:",y)] -> arrayDo x y
        _ -> throwError "evalKeywordMessage: Array?"
    ClassObject x ->
      case (x,keywordArguments) of
        ("Interval",[("from:",p1),("to:",p2)]) -> arrayFromTo p1 p2
        ("Interval",[("from:",p1),("to:",p2),("by:",p3)]) -> arrayFromToBy p1 p2 p3
        ("MRG",[("lhs:",p1),("rhs:",p2)]) -> liftUGen2 SC3.mrg2 p1 p2
        ("Splay",[("input:",p1)]) -> liftUGen (\input -> SC3.splay input 1 1 0 True) p1
        ("Control",[("name:",p1),("init:",p2)]) -> controlInput p1 p2
        --("Splay",[("input:",p1),("spread:",p2),("level:",p3),("center:",p4),("levelComp:",p5)]) -> liftUGen6 SC3.splay o p1 p2 p3 p4 p5
        _ -> throwError ("evalKeywordMessage: ClassObject: " ++ x)
    _ -> throwError "evalKeywordMessage"

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
    _ -> throwError "eval_basicexpression?"

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
  [("true",UGenObject (SC3.int_to_ugen 1))
  ,("false",UGenObject (SC3.int_to_ugen 0))]

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

initialEnvironment :: IO (Env.Env Object)
initialEnvironment = Env.env_gen_toplevel coreDict

replInit :: IO ()
replInit = do
  env <- initialEnvironment
  replCont env

{-
-- > evalSmalltalkFile "/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st"
evalSmalltalkFile :: FilePath -> IO ()
evalSmalltalkFile fn = do
  str <- readFile fn
  env <- initialEnvironment
  (r,_) <- runStateT (runExceptT (evalString str)) env
  case r of
    Left msg -> putStrLn ("error: " ++ msg)
    Right res -> putStrLn ("result: " ++ show res)
-}

{-
liftUGen6 :: (SC3.UGen -> SC3.UGen -> SC3.UGen -> SC3.UGen -> SC3.UGen -> SC3.UGen -> SC3.UGen) -> Object -> Object -> Object -> Object -> Object -> Object -> VM Object
liftUGen6 f p1 p2 p3 p4 p5 p6 = do
  u1 <- objectToUGen p1
  u2 <- objectToUGen p2
  u3 <- objectToUGen p3
  u4 <- objectToUGen p4
  u5 <- objectToUGen p5
  u6 <- objectToUGen p6
  return (UGenObject (f u1 u2 u3 u4 u5 u6))
-}
