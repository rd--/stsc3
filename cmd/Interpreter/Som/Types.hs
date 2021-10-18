{-# Language FlexibleContexts #-}

-- | Type definitions and functions over these.
module Interpreter.Som.Types where

import Control.Monad.IO.Class {- base -}
import qualified Data.Char {- base -}
import Data.IORef {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Data.Hashable as Hashable {- hashable -}
import qualified Data.Map as Map {- containers -}
import qualified Data.Time.Clock.System as Time {- time -}
import qualified Data.Vector as Vector {- vector -}

import qualified Control.Monad.State as State {- mtl -}
import qualified Control.Monad.Except as Except {- mtl -}

import qualified Sound.SC3.Lisp.Env as Env {- hsc3-lisp -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr as Expr {- stsc3 -}
import qualified Language.Smalltalk.Som as Som {- stsc3 -}

import Interpreter.Som.Int
import Interpreter.Som.Ref
import Interpreter.Som.Str
import Interpreter.Som.Sym
import Interpreter.Som.Tbl
import Interpreter.Som.Vec

-- | Extensible mutable dictionary of named objects.
type Dict = Env.DictRef Symbol Object

-- | Indexable mutable association list (zero-indexed) of named objects.
type Tbl = Vec (Symbol,IORef Object)

-- | Identifier.
type Id = Int

{- | Method contexts store:
       1. a context identifier to receive non-local returns
       2. the receiver
     Block contexts store:
       1. the Block object to report cases of escaped blocks.
     In addition both store local variables (arguments and temporaries) as a Dict.
-}
data ContextNode =
    MethodContext Id Object Dict
  | BlockContext Object Dict
  | NilContext
  deriving (Eq)

{- | A Context is the environment a Smalltalk expression is evaluated in.
     The Name lookup rules are:

     For methods: 1. temporaries & arguments,
                  2. receiver instance variables,
                  3. receiver class variables,
                  4. globals.

     For blocks:  1. temporaries & arguments,
                  2. parent context chain,
                  3. globals,
                  4. workspace.
-}
data Context = Context ContextNode (Maybe Context) deriving (Eq)

-- | Smalltalk expression
type StExpr = Expr.Expr ()

-- * Object

{- | Data associated with an object.

     Som:
       Som has no Character class
       Som strings are immutable
       Symbol is a subclass of String
-}
data ObjectData
  = DataNil
  | DataBoolean Bool
  | DataInteger LargeInteger
  | DataDouble Double
  | DataCharacter Char -- ^ Not in SOM
  | DataString UnicodeString
  | DataArray (IORef (Vec Object)) -- ^ Arrays are mutable
  | DataClass (St.ClassDefinition, Bool) Tbl (Vec Object,Vec Object) -- ^ Class definition and level, class variables, method caches
  | DataMethod Symbol St.MethodDefinition StExpr -- ^ Holder, definition, lambda StExpr
  | DataPrimitive Symbol Symbol -- ^ Holder & Signature
  | DataBlock Id Context StExpr -- ^ Identity, context, lambda StExpr
  | DataReturn Id (Maybe Object) Object -- ^ Return contextId, Block returned from & value
  | DataSystem -- ^ Token for System instance.
  | DataUser Id Tbl
  deriving (Eq)

-- | Object represented as class name and object data.
data Object = Object Symbol ObjectData deriving (Eq)

-- | Table of named objects.
type ObjectTable = [(Symbol,Object)]

-- | Primitive (receiver -> arguments -> result)
type Primitive = Object -> [Object] -> VM Object

-- | (Class,MethodSignature) -> Primitive table
type PrimitiveTable = [((Symbol,Symbol),Primitive)]

-- | Dictionary form of a PrimitiveTable
type PrimitiveDictionary = Map.Map (Symbol,Symbol) Primitive

{- | The VM state holds:
     - startTime, required for System>>ticks and System>>time
     - programCounter, used to identify method contexts and non-immediate objects
     - context, holds the currently executing context
     - globalDictionary, holds global variables
     - workspaceDictionary, holds workspace variables
-}
type VMState = (Double,Int,Context,Dict,Dict)

-- | VM is an Exception/State monad over VMState
type VM r = Except.ExceptT String (State.StateT VMState IO) r

-- | Generate VM state from initial global dictionary.
vmStateInit :: Dict -> IO VMState
vmStateInit globalDictionary = do
  startTime <- getSystemTimeAsDouble
  let programCounter = 0
  workspace <- Env.dictRefEmpty
  return (startTime,programCounter,nilContext,globalDictionary,workspace)

-- | Alias for Except.throwError
vmError :: String -> VM t
vmError = Except.throwError

-- | Fetch start time.
vmStartTime :: VM Double
vmStartTime = State.get >>= \(startTime,_,_,_,_) -> return startTime

-- | Get current system time as a floating point value (in seconds).
getSystemTimeAsDouble :: MonadIO m => m Double
getSystemTimeAsDouble = do
  tm <- liftIO Time.getSystemTime
  return (fromIntegral (Time.systemSeconds tm) + (fromIntegral (Time.systemNanoseconds tm) * 1.0e-9))

-- | Get elapsed system time in microseconds (us).
vmSystemTicksInt :: VM Int
vmSystemTicksInt = do
  startTime <- vmStartTime
  currentTime <- getSystemTimeAsDouble
  return (round ((currentTime - startTime) * 1.0e6))

-- | Fetch program counter.
vmProgramCounter :: VM Int
vmProgramCounter = State.get >>= \(_,programCounter,_,_,_) -> return programCounter

-- | Increment program counter and return previous (pre-increment) value.
vmProgramCounterIncrement :: VM Int
vmProgramCounterIncrement = State.get >>= \(tm,pc,ctx,glb,usr) -> State.put (tm,pc + 1,ctx,glb,usr) >> return pc

-- | Fetch current context
vmContext :: VM Context
vmContext = State.get >>= \(_,_,ctx,_,_) -> return ctx

-- | Fetch Id of Method context, else error.
vmContextId :: VM Id
vmContextId = vmContext >>= \ctx -> maybe (vmError "vmContextId: lookup failed") return (contextIdMaybe ctx)

-- | Fetch current block, else Nothing
vmContextCurrentBlock :: VM (Maybe Object)
vmContextCurrentBlock = fmap contextCurrentBlock vmContext

-- | Apply /f/ at the context and store the result.
vmContextModify :: (Context -> VM Context) -> VM Object
vmContextModify f = do
  (tm,pc,ctx,glb,usr) <- State.get
  modifiedCtx <- f ctx
  State.put (tm,pc,modifiedCtx,glb,usr)
  return nilObject

-- | Add a frame to the context.
vmContextAdd :: ContextNode -> VM ()
vmContextAdd x = vmContextModify (\ctx -> return (contextAdd ctx x)) >> return ()

-- | Delete a frame from the context
vmContextDelete :: VM ()
vmContextDelete = vmContextModify contextDelete >> return ()

-- | Replace the context, return the previous context.
vmContextReplace :: Context -> VM Context
vmContextReplace ctx = do
  (tm,pc,previousCtx,glb,usr) <- State.get
  State.put (tm,pc,ctx,glb,usr)
  return previousCtx

-- | Fetch global dictionary
vmGlobalDict :: VM Dict
vmGlobalDict = State.get >>= \(_,_,_,dict,_) -> return dict

-- | Lookup global, don't attempt to resolve if not found.
vmGlobalLookupMaybe :: Symbol -> VM (Maybe Object)
vmGlobalLookupMaybe key = vmGlobalDict >>= \dict -> Env.dictRefLookup dict key

-- | Lookup global, don't attempt to resolve if not found, return nil if not found.
vmGlobalLookupOrNil :: Symbol -> VM Object
vmGlobalLookupOrNil key = vmGlobalLookupMaybe key >>= return . fromMaybe nilObject

-- | Lookup global, don't attempt to resolve if not found, error if not found.
vmGlobalLookupOrError :: Symbol -> VM Object
vmGlobalLookupOrError key = vmGlobalLookupMaybe key >>= maybe (vmError ("vmGlobalLookup: " ++ fromSymbol key)) return

-- | Is global assigned, don't attempt to resolve if not.
vmHasGlobal :: Symbol -> VM Bool
vmHasGlobal = fmap (maybe False (const True)) . vmGlobalLookupMaybe

-- | Assign to existing global variable.
vmGlobalAssignMaybe :: Symbol -> Object -> VM (Maybe Object)
vmGlobalAssignMaybe key value = do
  d <- vmGlobalDict
  Env.dictRefAssignMaybe d key value

-- | Assign to or create new global variable.
vmGlobalAssign :: Symbol -> Object -> VM Object
vmGlobalAssign key value = vmGlobalDict >>= \d -> Env.dictRefInsert d key value >> return value

-- | Fetch workspace dictionary
vmWorkspaceDict :: VM Dict
vmWorkspaceDict = State.get >>= \(_,_,_,_,d) -> return d

vmWorkspaceLookupMaybe :: Symbol -> VM (Maybe Object)
vmWorkspaceLookupMaybe key = vmWorkspaceDict >>= \dict -> Env.dictRefLookup dict key

-- | Assign to existing workspace variable.
vmWorkspaceAssignMaybe :: Symbol -> Object -> VM (Maybe Object)
vmWorkspaceAssignMaybe key value = do
  d <- vmWorkspaceDict
  Env.dictRefAssignMaybe d key value

-- | Assign to existing workspace variable or allocate new variable.
vmWorkspaceInsert :: Symbol -> Object -> VM Object
vmWorkspaceInsert key value = do
  d <- vmWorkspaceDict
  Env.dictRefInsert d key value
  return value

-- | System>>inspect
vmShowDetailed :: VM String
vmShowDetailed = do
  (_tm,pc,_ctx,glb,wrk) <- State.get
  globalKeys <- Env.dictRefKeys glb
  workspaceKeys <- Env.dictRefKeys wrk
  return (show ("programCounter",pc,"global",globalKeys,"workspace",workspaceKeys))

-- * Printing

-- | Concise object printer.
objectToString :: Object -> String
objectToString (Object nm obj) =
  case obj of
    DataNil -> "nil"
    DataBoolean x -> map Data.Char.toLower (show x)
    DataInteger x -> show x
    DataDouble x -> show x
    DataCharacter x -> ['$',x]
    DataString x ->
      if nm == toSymbol "Symbol"
      then concat ["#'",fromUnicodeString x,"'"]
      else concat ["'",fromUnicodeString x,"'"]
    DataArray _ -> "instance of Array"
    DataClass (x,isMeta) _ _ -> (if isMeta then St.metaclassName else id) (St.className x)
    DataMethod holder method _ -> concat [fromSymbol holder,">>",St.methodSignature method]
    DataPrimitive holder signature -> concat ["Primitive:",fromSymbol holder,">>",fromSymbol signature]
    DataBlock _ _ _ -> "instance of " ++ fromSymbol nm
    DataReturn _ _ o -> "Return: " ++ objectToString o
    DataSystem -> "instance of " ++ fromSymbol nm
    DataUser _ _ -> "instance of " ++ fromSymbol nm

instance Show Object where show = objectToString

objectPrint :: MonadIO m => Object -> m Object
objectPrint o = liftIO (putStrLn (objectToString o)) >> return nilObject

objectListPrint :: MonadIO m => [Object] -> m Object
objectListPrint o = liftIO (putStrLn (intercalate ", " (map objectToString o))) >> return nilObject

-- * Inspect

-- | Inspect instance variables.
tblToInspector :: Tbl -> VM String
tblToInspector tbl = do
  (keys,values) <- fmap unzip (tblToList tbl)
  valuesInspected <- mapM objectToInspector values
  return (show (zip keys valuesInspected))

-- | Object>>inspect
objectToInspector :: Object -> VM String
objectToInspector (Object nm obj) =
  case obj of
    DataArray ref -> do
      x <- deRef ref
      let l = map objectToString (Vector.toList x)
      return (concat ["{",intercalate ". " l,"}"])
    DataClass (x,_) tbl _ -> do
      tblStr <- tblToInspector tbl
      return (St.className x ++ ": " ++ tblStr)
    DataMethod _ x _ -> return (show x)
    DataBlock x _ (Expr.Lambda ld _ _ _) ->
      return (printf "instance of %s <pc:%d, %s>" nm x (Expr.lambdaDefinitionShow ld))
    DataUser x tbl -> do
      tblStr <- tblToInspector tbl
      return (printf "instance of %s <pc:%d>: %s" nm x tblStr)
    DataSystem -> vmShowDetailed
    _ -> return (objectToString (Object nm obj))

-- * Error

objectError :: Object -> String -> VM t
objectError o msg = objectPrint o >> vmError msg

objectListError :: [Object] -> String -> VM Object
objectListError o msg = objectListPrint o >> vmError (printf "%s: arity=%d" msg (length o))

-- * Accessors

classMethodsVec :: Object -> Maybe (Vec Object)
classMethodsVec (Object _ obj) =
  case obj of
    DataClass (_,False) _ (instanceMethodsCache,_) -> Just instanceMethodsCache
    DataClass (_,True) _ (_,classMethodsCache) -> Just classMethodsCache
    _ -> Nothing

arrayElements :: Object -> VM [Object]
arrayElements o = case o of
  Object _ (DataArray vectorRef) -> fmap Vector.toList (deRef vectorRef)
  _ -> vmError ("arrayElements: not array")

-- | Lookup instance variable of Object.
objectLookupInstanceVariable :: Object -> Symbol -> VM (Maybe Object)
objectLookupInstanceVariable o key =
  case o of
    Object _ (DataUser _ tbl) -> tblAtKeyMaybe tbl key
    _ -> return Nothing

-- | Assign to instance variable of Object.
objectAssignInstanceVariable :: Object -> Symbol -> Object -> VM (Maybe Object)
objectAssignInstanceVariable object key value =
  case object of
    Object _ (DataUser _ tbl) -> tblAtKeyPutMaybe tbl key value
    _ -> return Nothing

-- * Object constructors

-- | Make reserved identifier object.  These are stored in the global dictionary.
reservedObject :: String -> Object
reservedObject x =
  case x of
    "true" -> Object (toSymbol "True") (DataBoolean True)
    "false" -> Object (toSymbol "False") (DataBoolean False)
    "nil" -> Object (toSymbol "Nil") DataNil
    "system" -> Object (toSymbol "System") DataSystem
    _ -> error "reservedObject"

-- | nil
nilObject :: Object
nilObject = reservedObject "nil"

-- | Make class and instance method caches.
classMethodCache :: St.ClassDefinition -> (Vec Object,Vec Object)
classMethodCache cd =
  let im = map (methodObject (toSymbol (St.className cd))) (St.instanceMethods cd)
      cm = map (methodObject (toSymbol (St.classMetaclassName cd))) (St.classMethods cd)
  in (Vector.fromList im,Vector.fromList cm)

-- | A Tbl with all variables set to nil.
variablesTbl :: MonadIO m => [Symbol] -> m Tbl
variablesTbl variableNames = tblFromList (zip variableNames (repeat nilObject))

{- | Create Class object from ClassDefinition
     The instance and class methods are generated and cached.
-}
classObject :: MonadIO m => St.ClassDefinition -> m Object
classObject cd = do
  let classVarNames = map toSymbol (St.classVariableNames cd)
  tbl <- variablesTbl classVarNames
  return (Object
           (toSymbol (St.classMetaclassName cd))
           (DataClass (cd,False) tbl (classMethodCache cd)))

{- | Create method Object for named Class.
     This is the point at which the MethodDefinition is translated to Expr form.
-}
methodObject :: Symbol -> St.MethodDefinition -> Object
methodObject holder method =
  Object (toSymbol "Method") (DataMethod holder method (Expr.methodDefinitionExpr method))

integerObject :: LargeInteger -> Object
integerObject x = Object (toSymbol "Integer") (DataInteger x)

doubleObject :: Double -> Object
doubleObject x = Object (toSymbol "Double") (DataDouble x)

characterObject :: Char -> Object
characterObject x = Object (toSymbol "Character") (DataCharacter x)

unicodeStringObject :: UnicodeString -> Object
unicodeStringObject x = Object (toSymbol "String") (DataString x)

stringObject :: String -> Object
stringObject = unicodeStringObject . toUnicodeString . Som.somEscapedString

unicodeSymbolObject :: UnicodeString -> Object
unicodeSymbolObject x = Object (toSymbol "Symbol") (DataString x)

symbolObject :: String -> Object
symbolObject = unicodeSymbolObject . toUnicodeString . Som.somEscapedString

booleanObject :: Bool -> Object
booleanObject x = if x then reservedObject "true" else reservedObject "false"

falseObject :: Object
falseObject = booleanObject False

trueObject :: Object
trueObject = booleanObject True

arrayFromVec :: MonadIO m => Vec Object -> m Object
arrayFromVec e = do
  a <- liftIO (newIORef e)
  return (Object (toSymbol "Array") (DataArray a))

arrayFromList :: MonadIO m => [Object] -> m Object
arrayFromList e = arrayFromVec (Vector.fromList e)

literalObject :: MonadIO m => St.Literal -> m Object
literalObject l =
  case l of
    St.NumberLiteral (St.Int x) -> return (integerObject x)
    St.NumberLiteral (St.Float x) -> return (doubleObject x)
    St.StringLiteral x -> return (stringObject x)
    St.CharacterLiteral x -> return (characterObject x) -- ? Som has no Character object
    St.SymbolLiteral x -> return (symbolObject x)
    St.SelectorLiteral x -> return (symbolObject (St.selectorIdentifier x))
    St.ArrayLiteral x -> arrayFromList =<< mapM arrayLiteralElemObject x

arrayLiteralElemObject :: MonadIO m => Either St.Literal String -> m Object
arrayLiteralElemObject e =
  case e of
    Left x -> literalObject x
    Right x -> return (reservedObject x)

{- | Mark an Object as being a Return Object (from a Block or Method).
     Include the contextId the value is returning to,
     and the Block that is returning (if it is a Block and not a Method).
     It is an error if the object returned is already Return Object.
-}
returnObject :: Except.MonadError String m => Id -> Maybe Object -> Object -> m Object
returnObject pc blockObject x =
  if isReturnObject x
  then Except.throwError "returnObject: Return"
  else return (Object (toSymbol "Return") (DataReturn pc blockObject x))

-- * Object predicates

isReturnObject :: Object -> Bool
isReturnObject x =
  case x of
    Object _ (DataReturn _ _ _) -> True
    _ -> False

isNil :: Object -> Bool
isNil = (==) nilObject

-- * Context

{- | Dictionary of arguments and temporaries.
     Temporaries may shadow arguments.
     Dict will discard multiple keys, keeping the last assigned.
     Therefore arguments are set first.
-}
localVariablesDict :: MonadIO m => ObjectTable -> [Symbol] -> m Dict
localVariablesDict args tmp = Env.dictRefFromList (args ++ zip tmp (repeat nilObject))

methodContextNode :: MonadIO m => Id -> Object -> ObjectTable -> St.Temporaries -> m ContextNode
methodContextNode pc rcv arg (St.Temporaries tmp) =
  fmap (MethodContext pc rcv) (localVariablesDict arg (map toSymbol tmp))

-- | The empty context.  It is ordinarily an error to encounter this.
nilContext :: Context
nilContext = Context NilContext Nothing

-- | Traverse context to innermost MethodContext and get receiver.
contextReceiverMaybe :: Context -> Maybe Object
contextReceiverMaybe (Context c p) =
  case c of
    BlockContext _ _ -> maybe Nothing contextReceiverMaybe p
    MethodContext _ rcv _ -> Just rcv
    NilContext -> Nothing

-- | Traverse context to innermost MethodContext and get Id.
contextIdMaybe :: Context -> Maybe Id
contextIdMaybe (Context c p) =
  case c of
    BlockContext _ _ -> maybe Nothing contextIdMaybe p
    MethodContext pc _ _ -> Just pc
    NilContext -> Nothing

-- | Does Context have a Method with Id?
contextHasId :: Id -> Context -> Bool
contextHasId k (Context c p) =
  case c of
    BlockContext _ _ -> maybe False (contextHasId k) p
    MethodContext pc _ _ -> pc == k || maybe False (contextHasId k) p
    NilContext -> False

-- | Get the blockObject from the current frame.
contextCurrentBlock :: Context -> Maybe Object
contextCurrentBlock (Context c _x) =
  case c of
    BlockContext blockObject _ -> Just blockObject
    MethodContext _ _ _ -> Nothing
    NilContext -> Nothing

-- | Add a node to the start of the Context.
contextAdd :: Context -> ContextNode -> Context
contextAdd ctx nd = Context nd (Just ctx)

{- | Deleting a context with no parent is an error.
     (Root contexts ought to have the NilContext as a parent.)
-}
contextDelete :: Except.MonadError String m => Context -> m Context
contextDelete ctx =
  case ctx of
    Context _ (Just p) -> return p
    Context _ Nothing -> Except.throwError "contextDelete: empty context"

-- * Hash

mHash :: (Monad m,Hashable.Hashable t) => t -> m LargeInteger
mHash = return . toLargeInteger . Hashable.hash

-- | Hash of object.  Used for object equality.
objectIntHash :: (MonadIO m, Except.MonadError String m) => Object -> m LargeInteger
objectIntHash (Object nm obj) =
  case obj of
    DataNil -> mHash (nm,"nil")
    DataBoolean x -> mHash x
    DataInteger x -> return x -- c.f. Integer>>hashcode
    DataDouble x -> mHash x
    DataCharacter x -> mHash x
    DataString x -> mHash x -- c.f. 'x' hashcode = #'x' hashcode
    DataArray x -> deRef x >>= \vec -> mapM objectIntHash (Vector.toList vec) >>= mHash
    DataClass (x,_) _ _ -> mHash (nm,St.className x)
    DataMethod holder method _ -> mHash (nm,holder,St.methodSignature method)
    DataPrimitive holder signature -> mHash (nm,holder,signature)
    DataBlock x _ _ -> mHash ("Block",x)
    DataReturn _ _ _ -> Except.throwError ("Object>>hashcode: Return")
    DataSystem -> mHash (nm,"system")
    DataUser x _ -> mHash (nm,x)
