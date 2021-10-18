-- | Primitives that do not require access to interpreter state.
module Interpreter.Som.Primitives where

import Control.Monad.IO.Class {- base -}
import Data.Bits {- base -}
import qualified Data.Char {- base -}
import Data.Fixed {- base -}
import Data.List {- base -}
import System.Directory {- directory -}
import System.Mem {- base -}
import System.Random {- random -}

import qualified Data.Text as Text {- text -}
import qualified Data.Text.IO as Text.IO {- text -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

import Interpreter.Som.Int
import Interpreter.Som.Ref
import Interpreter.Som.Str
import Interpreter.Som.Sym
import Interpreter.Som.Tbl
import Interpreter.Som.Types
import Interpreter.Som.Vec

-- | Alias for Except.throwError
prError :: String -> VM t
prError = vmError

-- * Primitives for Array

-- | Array>>at:
arrayAt :: Primitive
arrayAt (Object nm obj) arg = case (obj,arg) of
  (DataArray ref,[Object _ (DataInteger ix)]) -> do
    v <- deRef ref
    if ix <= vecLength v
      then return (vecAt v (ix - 1))
      else prError "Array>>at: index out of range"
  _ -> prError ("Array>>at: " ++ fromSymbol nm)

-- | Array>>at:put: (Returns object put)
arrayAtPut :: Primitive
arrayAtPut (Object nm obj) arg = case (obj,arg) of
  (DataArray ref,[Object _ (DataInteger ix),newObject]) -> vecRefWrite ref (ix - 1) newObject
  _ -> prError ("Array>>at:put: " ++ fromSymbol nm)

-- | Array>>length
arrayLength :: Primitive
arrayLength (Object nm obj) arg = case (obj,arg) of
  (DataArray ref,[]) -> deRef ref >>= \v -> return (integerObject (vecLength v))
  _ -> prError ("Array>>length " ++ fromSymbol nm)

-- | Array class>>new:
arrayNew :: Primitive
arrayNew (Object nm obj) arg = case (obj,arg) of
  (DataClass _ _ _,[Object _ (DataInteger size)]) -> arrayFromList (genericReplicate size nilObject)
  _ -> prError ("Array>>new: " ++ fromSymbol nm)

-- * Primitives for Block

-- | Block>>restart
blockRestart :: Primitive
blockRestart (Object nm obj) arg = case (obj,arg) of
  (DataBlock _ _ _,[]) -> prError "Block>>restart not implemented"
  _ -> prError ("Block>>restart " ++ fromSymbol nm)

-- | Block>>value
blockValue :: Primitive
blockValue (Object nm obj) arg = case (obj,arg) of
  (DataBlock _ _ _,[]) -> prError "Block>>value not implemented"
  _ -> prError ("Block>>value " ++ fromSymbol nm)

-- * Primitives for Class

-- | Class>>methods => Array[Method]
classMethods :: Primitive
classMethods (Object nm obj) arg =
  case arg of
    [] -> maybe (prError "Class>>methods") arrayFromVec (classMethodsVec (Object nm obj))
    _ -> prError ("Class>>methods " ++ fromSymbol nm)

-- | Class>>name => Symbol
className :: Primitive
className (Object nm obj) arg =
  case (obj,arg) of
    (DataClass (cd,isMeta) _ _,[]) ->
      return (symbolObject ((if isMeta then St.metaclassName else id) (St.className cd)))
    _ -> prError ("Class>>name " ++ fromSymbol nm)

-- * Primitives for Double

-- | Double>>asString
doubleAsString :: Primitive
doubleAsString = unaryPrimitive "Double>>asString" (\arg -> case arg of
  DataDouble x -> Just (stringObject (show x))
  _ -> Nothing)

-- | Double>>PositiveInfinity
doublePositiveInfinity :: Primitive
doublePositiveInfinity o arg = case (o,arg) of
  (Object "Double class" (DataClass _ _ _),[]) -> return (doubleObject (read "Infinity"))
  _ -> prError "Double>>PositiveInfinity"

-- | Double class>>fromString: (String|Symbol -> Double)
doubleFromString :: Primitive
doubleFromString = binaryPrimitive "Double class>>fromString:" (\arg1 arg2 -> case (arg1,arg2) of
  (DataClass _ _ _,DataString x) -> fmap doubleObject (unicodeStringReadDouble x)
  _ -> Nothing)

-- * Primitives for Integer

-- | Integer>>asString
integerAsString :: Primitive
integerAsString = unaryPrimitive "Integer>>asString" (\arg -> case arg of
  DataInteger x -> Just (stringObject (show x))
  _ -> Nothing)

-- | Integer class>>fromString: (String|Symbol -> Integer)
integerFromString :: Primitive
integerFromString = binaryPrimitive "Integer class>>fromString:" (\arg1 arg2 -> case (arg1,arg2) of
  (DataClass _ _ _,DataString x) -> fmap integerObject (unicodeStringReadInteger x)
  _ -> Nothing)

objectAsDouble :: Object -> VM Double
objectAsDouble o = case o of
  Object _ (DataInteger x) -> return (fromIntegral x)
  Object _ (DataDouble x) -> return x
  _ -> prError "objectAsDouble"

{-
numberReduce :: Object -> VM Object
numberReduce o = case o of
  Object _ (DataInteger _) -> o
  Object _ (DataDouble x) -> doubleAsFractional x
  _ -> prError "numberReduce"
-}

-- | Integer>>//
integerFractionalDivision :: Primitive
integerFractionalDivision = numNumNumPrimitive "Integer>>//" (/)

-- | Integer>>sqrt
integerSquareRoot :: Primitive
integerSquareRoot = numNumPrimitive "Integer>>sqrt" sqrt

-- | Integer>>atRandom ; (0,n-1)
integerAtRandom :: Primitive
integerAtRandom o arg = case (o,arg) of
  (Object _ (DataInteger x),[]) -> fmap integerObject (liftIO (getStdRandom (randomR (0, x - 1))))
  _ -> prError "Integer>>atRandom"

-- * Primitives for Method

-- | Method>>signature => Symbol
methodSignature :: Primitive
methodSignature (Object nm obj) arg = case (obj,arg) of
  (DataMethod _ mth _,[]) -> return (symbolObject (St.selectorIdentifier (St.methodSelector mth)))
  _ -> prError ("Method>>signature " ++ fromSymbol nm)

-- * Primitives for Object

-- | Object>>== => Boolean
objectEqual :: Primitive
objectEqual rcv arg = case arg of
  [arg1] -> do
    hash1 <- objectIntHash rcv
    hash2 <- objectIntHash arg1
    return (booleanObject (hash1 == hash2))
  _ -> vmError ("Object>>== arity error")

-- | Object>>hashcode => Integer
objectHashcode :: Primitive
objectHashcode rcv arg = case arg of
  [] -> fmap integerObject (objectIntHash rcv)
  _ -> vmError ("Object>>hashcode: arity error")

-- | Object>>instVarAt: (one-indexed)
objectInstVarAt :: Primitive
objectInstVarAt rcv arg = case (rcv,arg) of
  (Object _ (DataUser _ tbl),[Object "Integer" (DataInteger ix)]) ->
    tblAtDefault tbl (fromLargeInteger ix - 1) (objectError rcv "Object>>instVarAt: index error")
  _ -> objectError rcv "Object>>instVarAt:"

-- | Object>>instVarAt:put: (one-indexed)
objectInstVarAtPut :: Primitive
objectInstVarAtPut rcv arg = case (rcv,arg) of
  (Object _ (DataUser _ tbl),[Object "Integer" (DataInteger ix),newObject]) ->
    tblAtPutDefault tbl (fromLargeInteger ix - 1) newObject (objectError rcv "Object>>instVarAt:put: index error")
  _ -> objectError rcv "Object>>instVarAt:put:"

-- | Object>>instVarNamed:
objectInstVarNamed :: Primitive
objectInstVarNamed rcv arg = case (rcv,arg) of
  (Object _ (DataUser _ tbl),[Object "Symbol" (DataString key)]) ->
    tblAtKeyDefault tbl (fromUnicodeString key) (objectError rcv "Object>>instVarNamed: no such key")
  _ -> objectError rcv "Object>>instVarNamed:"

-- * Primitives for Primitive

-- | Primitive>>holder
primitiveHolder :: Primitive
primitiveHolder (Object nm obj) arg = case (obj,arg) of
  (DataPrimitive x _,[]) -> return (symbolObject x)
  _ -> prError ("Primitive>>holder " ++ fromSymbol nm)

-- | Primitive>>signature
primitiveSignature :: Primitive
primitiveSignature (Object nm obj) arg = case (obj,arg) of
  (DataPrimitive _ x,[]) -> return (symbolObject x)
  _ -> prError ("Primitive>>signature " ++ fromSymbol nm)

-- * Primitives for String

-- | String>>asSymbol (String|Symbol -> Symbol)
stringAsSymbol :: Primitive
stringAsSymbol = unaryPrimitive "String>>asSymbol" (\arg1 -> case arg1 of
  DataString str1 -> Just (unicodeSymbolObject str1)
  _ -> Nothing)

-- | String>>concatenate:  (String|Symbol -> String|Symbol -> String)
stringConcatenate :: Primitive
stringConcatenate = binaryPrimitive "String>>concatenate:" (\arg1 arg2 -> case (arg1,arg2) of
  (DataString str1,DataString str2) -> Just (unicodeStringObject (Text.append str1 str2))
  _ -> Nothing)

-- | String>>= (String|Symbol -> String|Symbol -> Bool)
stringEqual :: Primitive
stringEqual = binaryPrimitive "String>>=" (\arg1 arg2 -> case (arg1,arg2) of
  (DataString str1, DataString str2) -> Just (booleanObject (str1 == str2))
  (DataString _,_) -> Just falseObject
  _ -> Nothing)

-- | String>>hashcode
stringHashcode :: Primitive
stringHashcode = objectHashcode

-- | String>>length (String|Symbol -> Int)
stringLength :: Primitive
stringLength = unaryPrimitive "String>>length" (\arg1 -> case arg1 of
  DataString str1 -> Just (integerObject (toLargeInteger (Text.length str1)))
  _ -> Nothing)

-- | Basis for isLetters and isDigits and isWhiteSpace.  Null strings are false.
stringAll :: (Char -> Bool) -> Primitive
stringAll f = unaryPrimitive "stringAll" (\arg1 -> case arg1 of
  DataString str1 -> Just (booleanObject (not (Text.null str1) && Text.all f str1))
  _ -> Nothing)

-- | String>>primSubstringFrom:to: (String|Symbol -> Int -> Int -> String)
stringPrimSubstringFromTo :: Primitive
stringPrimSubstringFromTo = ternaryPrimitive "String>>primSubstringFrom:to:" (\arg1 arg2 arg3 -> case (arg1,arg2,arg3) of
  (DataString str1,DataInteger int1,DataInteger int2) ->
    Just (unicodeStringObject (unicodeStringSubstringFromTo str1 int1 int2))
  _ -> Nothing)

-- * Primitives for Symbol

-- | Symbol>>asString
symbolAsString :: Primitive
symbolAsString = unaryPrimitive "Symbol>>asString" (\arg1 -> case arg1 of
  DataString str1 -> Just (unicodeStringObject str1)
  _ -> Nothing)

-- * Primitives for System

{-
import System.Exit {- base -}
-- | System>>exit: (exit process)
systemExitProcess :: Primitive
systemExitProcess (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[Object _ (DataInteger x)]) ->
    liftIO (if x == 0 then exitSuccess else exitWith (ExitFailure x))
  _ -> prError ("System>>exit: " ++ fromSymbol nm)
-}

-- | System>>exit: (exit doit)
systemExit :: Primitive
systemExit (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[Object _ (DataInteger x)]) ->
    prError ("exit: " ++ show x)
  _ -> prError ("System>>exit: " ++ fromSymbol nm)

-- | System>>fullGC:
systemFullGC :: Primitive
systemFullGC (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[]) -> liftIO System.Mem.performMajorGC >> return trueObject
  _ -> prError ("System>>fullGC: " ++ fromSymbol nm)

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe fn = do
  exists <- doesFileExist fn
  if exists then fmap Just (readFile fn) else return Nothing

{- | System>>global: (Symbol -> Object ; nil if value not bound)

> (system global: #Nil) = Nil                     "=> true"
> (system global: #UnknownObject) = nil           "=> true"
-}
systemGlobal :: Primitive
systemGlobal (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[Object "Symbol" (DataString x)]) -> vmGlobalLookupOrNil (Text.unpack x)
  _ -> prError ("System>>global: " ++ fromSymbol nm)

-- | System>>global:put:
systemGlobalPut :: Primitive
systemGlobalPut (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[Object "Symbol" (DataString x),e]) -> vmGlobalAssign (Text.unpack x) e
  _ -> prError ("System>>global:put: " ++ fromSymbol nm)

{- | System>>hasGlobal:

> (system global: #Nil) = Nil                     "=> true"
-}
systemHasGlobal :: Primitive
systemHasGlobal (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[Object "Symbol" (DataString x)]) -> fmap booleanObject (vmHasGlobal (Text.unpack x))
  _ -> prError ("System>>hasGlobal: " ++ fromSymbol nm)

{- | System>>loadFile: (String|Symbol -> String|Error)

> system loadFile: '/etc/passwd'
-}
systemLoadFile :: Primitive
systemLoadFile (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[Object _ (DataString x)]) -> do
    maybeText <- liftIO (readFileMaybe (Text.unpack x))
    maybe (prError "System>>loadFile: file does not exist") (return . stringObject) maybeText
  _ -> prError ("System>>loadFile: " ++ fromSymbol nm)

-- | System>>printString: (String|Symbol -> ())
systemPrintString :: Primitive
systemPrintString (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[Object _ (DataString x)]) -> liftIO (Text.IO.putStr x) >> return nilObject
  _ -> prError ("System>>printString: " ++ fromSymbol nm)

-- | System>>printNewline
systemPrintNewline :: Primitive
systemPrintNewline (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[]) -> liftIO (putChar '\n') >> return nilObject
  _ -> prError ("System>>printNewline " ++ fromSymbol nm)

{- | System>>ticks (elapsed time in microseconds)

> fromIntegral (maxBound::Int) >= ((2::Integer) ^ 62) ; True
> (((maxBound::Int) `div` (10^6)) `div` (60 * 60 * 24 * 365)) == 292471
> (((2^64) `div` (10^6)) `div` (60 * 60 * 24 * 365)) == 584942
-}
systemTicks :: Primitive
systemTicks (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[]) -> fmap (integerObject . toLargeInteger) vmSystemTicksInt
  _ -> prError ("System>>ticks " ++ fromSymbol nm)

-- | System>>time (elapsed time in milliseconds)
systemTime :: Primitive
systemTime (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[]) -> fmap (integerObject . toLargeInteger . (`div` 1000)) vmSystemTicksInt
  _ -> prError ("System>>time " ++ fromSymbol nm)

-- * Table

primitiveTable :: PrimitiveTable
primitiveTable =
  map (\((c,m),f) -> ((toSymbol c,toSymbol m),f))
  [
  -- Array
   (("Array","at:"),arrayAt)
  ,(("Array","at:put:"),arrayAtPut)
  ,(("Array","length"),arrayLength)
  ,(("Array class","new:"),arrayNew)
  -- Block
  ,(("Block","restart"),blockRestart)
  ,(("Block","value"),blockValue)
  -- Class
  ,(("Class","methods"),classMethods)
  ,(("Class","name"),className)
  -- Double
  ,(("Double","+"),doubleNumDoublePrimitive "+" (+))
  ,(("Double","-"),doubleNumDoublePrimitive "-" (-))
  ,(("Double","*"),doubleNumDoublePrimitive "*" (*))
  --,(("Double","/"),doubleNumDoublePrimitive (/)) -- ? Som
  ,(("Double","//"),doubleNumDoublePrimitive "//" (/)) -- Som
  ,(("Double","%"),doubleNumDoublePrimitive "%" mod')
  ,(("Double","sqrt"),doubleDoublePrimitive "sqrt" sqrt)
  ,(("Double","round"),doubleIntPrimitive "round" round) -- Som (roundTowardPositive in IEEE 754-2008)
  ,(("Double","asInteger"),doubleIntPrimitive "asInteger" truncate) -- Som
  ,(("Double","cos"),doubleDoublePrimitive "cos" cos)
  ,(("Double","sin"),doubleDoublePrimitive "sin" sin)
  ,(("Double","="),doubleNumBoolPrimitive "=" (==))
  ,(("Double","<"),doubleNumBoolPrimitive "<" (<))
  ,(("Double","asString"),doubleAsString)
  ,(("Double class","PositiveInfinity"),doublePositiveInfinity)
  ,(("Double class","fromString:"),doubleFromString)
  -- Integer
  ,(("Integer","+"),intNumNumPrimitive "+" (+) (+))
  ,(("Integer","-"),intNumNumPrimitive "-" (-) (-))
  ,(("Integer","*"),intNumNumPrimitive "*" (*) (*))
  ,(("Integer","/"),intNumNumPrimitive "/" div (/)) -- ? Som 1/2=0
  ,(("Integer","//"),integerFractionalDivision) -- ? Som 1//2=0.5
  ,(("Integer","%"),intNumNumPrimitive "%" mod mod')
  ,(("Integer","rem:"),intNumNumPrimitive "rem:" rem undefined)
  ,(("Integer","sqrt"),integerSquareRoot)
  ,(("Integer","atRandom"),integerAtRandom)
  ,(("Integer","="),intNumBoolPrimitiveOr (return falseObject) "=" (==) (==))
  ,(("Integer","<"),intNumBoolPrimitive "<" (<) (<))
  ,(("Integer","&"),intIntIntPrimitive "&" (Data.Bits..&.))
  ,(("Integer","<<"),intIntIntPrimitive "<<" shiftLeft)
  ,(("Integer",">>>"),intIntIntPrimitive ">>" shiftRight)
  ,(("Integer","bitXor:"),intIntIntPrimitive "bitXor:" Data.Bits.xor)
  ,(("Integer","asString"),integerAsString)
  ,(("Integer","asDouble"),intDoublePrimitive "asDouble" fromIntegral)
  ,(("Integer","as32BitUnsignedValue"),intIntPrimitive "as32BitUnsignedValue" as32BitUnsignedValue)
  ,(("Integer","as32BitSignedValue"),intIntPrimitive "as32BitSignedValue" as32BitSignedValue)
  ,(("Integer class","fromString:"),integerFromString)
  -- Method
  ,(("Method","signature"),methodSignature)
  -- Object
  ,(("Object","=="),objectEqual)
  ,(("Object","hashcode"),objectHashcode)
  ,(("Object","instVarAt:"),objectInstVarAt)
  ,(("Object","instVarAt:put:"),objectInstVarAtPut)
  ,(("Object","instVarNamed:"),objectInstVarNamed)
  -- Primitive
  ,(("Primitive","holder"),primitiveHolder)
  ,(("Primitive","signature"),primitiveSignature)
  -- String
  ,(("String","asSymbol"),stringAsSymbol)
  ,(("String","concatenate:"),stringConcatenate)
  ,(("String","="),stringEqual)
  ,(("String","length"),stringLength)
  ,(("String","isWhiteSpace"),stringAll Data.Char.isSpace)
  ,(("String","isLetters"),stringAll Data.Char.isLetter)
  ,(("String","isDigits"),stringAll Data.Char.isDigit)
  ,(("String","primSubstringFrom:to:"),stringPrimSubstringFromTo)
  ,(("String","hashcode"),stringHashcode)
  -- Symbol
  ,(("Symbol","asString"),symbolAsString)
  -- System
  ,(("System","exit:"),systemExit)
  ,(("System","fullGC"),systemFullGC)
  ,(("System","global:"),systemGlobal)
  ,(("System","global:put:"),systemGlobalPut)
  ,(("System","hasGlobal:"),systemHasGlobal)
  ,(("System","loadFile:"),systemLoadFile)
  ,(("System","printString:"),systemPrintString)
  ,(("System","printNewline"),systemPrintNewline)
  ,(("System","ticks"),systemTicks)
  ,(("System","time"),systemTime)
  ]

-- * Primitive Constructors

unaryPrimitive :: String -> (ObjectData -> Maybe Object) -> Primitive
unaryPrimitive nm fun o@(Object _ obj) arg = case arg of
  [] -> case fun obj of
          Just r -> return r
          Nothing -> objectError o ("unaryPrimitive: " ++ nm)
  _ -> prError ("unaryPrimitive: arity: " ++ nm)

binaryPrimitiveOr :: VM Object -> String -> (ObjectData -> ObjectData -> Maybe Object) -> Primitive
binaryPrimitiveOr def nm fun o@(Object _ obj) arg = case arg of
  [Object _ p1] -> case fun obj p1 of
            Just r -> return r
            Nothing -> def
  _ -> objectListError (o : arg) ("binaryPrimitive: arity: " ++ nm)

binaryPrimitive :: String -> (ObjectData -> ObjectData -> Maybe Object) -> Primitive
binaryPrimitive nm fun o arg = binaryPrimitiveOr (objectListError (o : arg) ("binaryPrimitive" ++ nm)) nm fun o arg

ternaryPrimitive :: String -> (ObjectData -> ObjectData -> ObjectData -> Maybe Object) -> Primitive
ternaryPrimitive nm fun o@(Object _ obj) arg = case arg of
  [Object _ p1,Object _ p2] -> case fun obj p1 p2 of
               Just r -> return r
               Nothing -> objectError o ("ternaryPrimitive: " ++ nm)
  _ -> objectListError arg ("ternaryPrimitive: arity: " ++ nm)

doubleAsFractional :: Double -> Object
doubleAsFractional x =
  case properFraction x of
    (i,0) -> integerObject i
    _ -> doubleObject x

numNumPrimitive :: String -> (Double -> Double) -> Primitive
numNumPrimitive msg f rcv arg = case arg of
  [] -> do
    lhs <- objectAsDouble rcv
    return (doubleAsFractional (f lhs))
  _ -> prError msg

numNumNumPrimitive :: String -> (Double -> Double -> Double) -> Primitive
numNumNumPrimitive msg f rcv arg = case arg of
  [arg1] -> do
    lhs <- objectAsDouble rcv
    rhs <- objectAsDouble arg1
    return (doubleAsFractional (f lhs rhs))
  _ -> prError msg

intIntPrimitive :: String -> (LargeInteger -> LargeInteger) -> Primitive
intIntPrimitive nm fun = unaryPrimitive nm (\arg1 -> case arg1 of
  DataInteger p1 -> Just (integerObject (fun p1))
  _ -> Nothing)

intDoublePrimitive :: String -> (LargeInteger -> Double) -> Primitive
intDoublePrimitive nm fun = unaryPrimitive nm (\arg1 -> case arg1 of
  DataInteger p1 -> Just (doubleObject (fun p1))
  _ -> Nothing)

intIntIntPrimitive :: String -> (LargeInteger -> LargeInteger -> LargeInteger) -> Primitive
intIntIntPrimitive nm fun = binaryPrimitive nm (\arg1 arg2 -> case (arg1,arg2) of
  (DataInteger p1,DataInteger p2) -> Just (integerObject (fun p1 p2))
  _ -> Nothing)

intNumNumPrimitive :: String -> (LargeInteger -> LargeInteger -> LargeInteger) -> (Double -> Double -> Double) -> Primitive
intNumNumPrimitive nm fun1 fun2 = binaryPrimitive nm (\arg1 arg2 -> case (arg1,arg2) of
  (DataInteger p1,DataInteger p2) -> Just (integerObject (fun1 p1 p2))
  (DataInteger p1,DataDouble p2) -> Just (doubleObject (fun2 (fromIntegral p1) p2))
  _ -> Nothing)

intNumBoolPrimitiveOr :: VM Object -> String -> (LargeInteger -> LargeInteger -> Bool) -> (Double -> Double -> Bool) -> Primitive
intNumBoolPrimitiveOr def nm fun1 fun2 = binaryPrimitiveOr def nm (\arg1 arg2 -> case (arg1,arg2) of
  (DataInteger p1,DataInteger p2) -> Just (booleanObject (fun1 p1 p2))
  (DataInteger p1,DataDouble p2) -> Just (booleanObject (fun2 (fromIntegral p1) p2))
  _ -> Nothing)

intNumBoolPrimitive :: String -> (LargeInteger -> LargeInteger -> Bool) -> (Double -> Double -> Bool) -> Primitive
intNumBoolPrimitive nm fun1 fun2 = binaryPrimitive nm (\arg1 arg2 -> case (arg1,arg2) of
  (DataInteger p1,DataInteger p2) -> Just (booleanObject (fun1 p1 p2))
  (DataInteger p1,DataDouble p2) -> Just (booleanObject (fun2 (fromIntegral p1) p2))
  _ -> Nothing)

doubleDoublePrimitive :: String -> (Double -> Double) -> Primitive
doubleDoublePrimitive nm fun = unaryPrimitive nm (\arg1 -> case arg1 of
  DataDouble p1 -> Just (doubleObject (fun p1))
  _ -> Nothing)

doubleIntPrimitive :: String -> (Double -> LargeInteger) -> Primitive
doubleIntPrimitive nm fun = unaryPrimitive nm (\arg1 -> case arg1 of
  DataDouble p1 -> Just (integerObject (fun p1))
  _ -> Nothing)

doubleNumDoublePrimitive :: String -> (Double -> Double -> Double) -> Primitive
doubleNumDoublePrimitive nm fun = binaryPrimitive nm (\arg1 arg2 -> case (arg1,arg2) of
  (DataDouble p1,DataInteger p2) -> Just (doubleObject (fun p1 (fromIntegral p2)))
  (DataDouble p1,DataDouble p2) -> Just (doubleObject (fun p1 p2))
  _ -> Nothing)

doubleNumBoolPrimitive :: String -> (Double -> Double -> Bool) -> Primitive
doubleNumBoolPrimitive nm fun = binaryPrimitive nm (\arg1 arg2 -> case (arg1,arg2) of
  (DataDouble p1,DataInteger p2) -> Just (booleanObject (fun p1 (fromIntegral p2)))
  (DataDouble p1,DataDouble p2) -> Just (booleanObject (fun p1 p2))
  _ -> Nothing)
