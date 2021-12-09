{- | Variable lookup and assignment,
     expression evalulation,
     interpreter primitives -}
module Interpreter.Som.Core where

import Control.Monad {- base -}
import Control.Monad.IO.Class {- base -}
import qualified Data.Char {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Data.Map as Map {- containers -}
import qualified Data.Text as Text {- text -}
import qualified Data.Vector as Vector {- vector -}

import qualified Control.Monad.State as State {- mtl -}
import qualified Control.Monad.Except as Except {- mtl -}

import qualified Sound.SC3.Lisp.Env as Env {- hsc3-lisp -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr as Expr {- stsc3 -}
import qualified Language.Smalltalk.Som as Som {- stsc3 -}

import Interpreter.Som.Primitives
import Interpreter.Som.Str
import Interpreter.Som.Sym
import Interpreter.Som.Tbl
import Interpreter.Som.Types

-- * Lookup

-- | Sequence of lookup procedures to be tried in left to right sequence.
mLookupSequence :: Monad m => [k -> m (Maybe v)] -> k -> m (Maybe v)
mLookupSequence l k =
  case l of
    [] -> return Nothing
    f:l' -> do
      r <- f k
      case r of
        Nothing -> mLookupSequence l' k
        _ -> return r

-- | Sequence of assignment procedures to be tried in left to right sequence.
mAssignSequence :: Monad m => [k -> v -> m (Maybe v)] -> k -> v -> m (Maybe v)
mAssignSequence l k v =
  case l of
    [] -> return Nothing
    f:l' -> do
      r <- f k v
      case r of
        Nothing -> mAssignSequence l' k v
        _ -> return r

-- * Context

{- | Lookup class variable from Object. If the object is:
     1. a class then look in it's table, else lookup it's superclass.
     2. nil then stop looking
     3. any other object look in it's class object
-}
objectLookupClassVariable :: Object -> Symbol -> VM (Maybe Object)
objectLookupClassVariable object key =
  case object of
    Object _ DataNil -> return Nothing
    Object _ (DataClass _ tbl _) ->
      mLookupSequence [tblAtKeyMaybe tbl
                      ,\k -> classSuperclass object [] >>= \sp -> objectLookupClassVariable sp k] key
    _ -> objectClass object [] >>= \cl -> objectLookupClassVariable cl key

-- | Lookup a name in a Context.  See Context for description of lookup rules.
contextLookup :: Context -> Symbol -> VM (Maybe Object)
contextLookup (Context c p) k =
  case c of
    MethodContext _ rcv localVariables ->
      if k == "self" || k == "super"
      then return (Just rcv)
      else mLookupSequence [Env.dictRefLookup localVariables
                           ,objectLookupInstanceVariable rcv
                           ,objectLookupClassVariable rcv
                           ,vmGlobalResolveMaybe] k
    BlockContext _ localVariables ->
      mLookupSequence [Env.dictRefLookup localVariables
                      ,maybe (\_ -> return Nothing) (\c' -> contextLookup c') p
                      ,vmGlobalResolveMaybe
                      ,vmWorkspaceLookupMaybe] k
    NilContext ->
      mLookupSequence [vmGlobalResolveMaybe
                      ,vmWorkspaceLookupMaybe] k

-- | Assign to class variable of Object.  For rules see objectLookupClassVariable.
objectAssignClassVariable :: Object -> Symbol -> Object -> VM (Maybe Object)
objectAssignClassVariable object key value =
  case object of
    Object _ DataNil -> return Nothing
    Object _ (DataClass _ tbl _) ->
      mAssignSequence [tblAtKeyPutMaybe tbl
                      ,\k v -> classSuperclass object [] >>= \sp -> objectAssignClassVariable sp k v] key value
    _ -> objectClass object [] >>= \cl -> objectAssignClassVariable cl key value

{- | Set a name in a Context.
     Assignments at NilContext (the empty context) set variables in the Workspace.
-}
contextAssign :: Context -> Symbol -> Object -> VM (Maybe Object)
contextAssign (Context c p) k v =
  case c of
    MethodContext _ rcv localVariables ->
      mAssignSequence [Env.dictRefAssignMaybe localVariables
                      ,objectAssignInstanceVariable rcv
                      ,objectAssignClassVariable rcv
                      ,vmGlobalAssignMaybe] k v
    BlockContext _ localVariables ->
      mAssignSequence [Env.dictRefAssignMaybe localVariables
                      ,maybe (\_ _ -> return Nothing) (\c' -> contextAssign c') p
                      ,vmGlobalAssignMaybe
                      ,vmWorkspaceAssignMaybe] k v
    NilContext -> fmap Just (vmWorkspaceInsert k v)

{- | Add blockFrame to blockContext.
     For blocks with no arguments and no temporaries and no return statements,
     the context could be elided.
-}
contextAddBlockContext :: Object -> [Object] -> VM Context
contextAddBlockContext blockObject arguments = do
  let Object _ (DataBlock _ blockContext lambda) = blockObject
      Expr.Lambda _ blockArguments (St.Temporaries blockTemporaries) _ = lambda
  when (length blockArguments /= length arguments) (vmError "contextAddBlockContext: arity error")
  localVariables <- localVariablesDict (zip blockArguments arguments) blockTemporaries
  return (contextAdd blockContext (BlockContext blockObject localVariables))

-- | Lookup value in current context.
vmContextLookup :: Symbol -> VM Object
vmContextLookup k = do
  ctx <- vmContext
  res <- contextLookup ctx k
  maybe (vmUnknownGlobal ctx k) return res

-- | Assign value in current context
vmContextAssign :: Symbol -> Object -> VM Object
vmContextAssign k v = do
  ctx <- vmContext
  res <- contextAssign ctx k v
  maybe (vmError ("vmContextAssign: " ++ show k)) return res

-- | Run vmContextAssign to set all temporaries to Nil.
vmContextAssignAllToNil :: [Symbol] -> VM ()
vmContextAssignAllToNil = mapM_ (\name -> vmContextAssign name nilObject)

-- * VM

-- | When a method lookup fails, the doesNotUnderstand:arguments: message is sent to the receiver.
vmDoesNotUnderstand :: Object -> String -> Object -> VM Object
vmDoesNotUnderstand receiver k argsArray = do
  let sel = St.KeywordSelector "doesNotUnderstand:arguments:"
  evalMessageSend False receiver sel [symbolObject k, argsArray]

-- | When a global lookup fails, the unknownGlobal: message is sent to the contextReceiver, if there is one.
vmUnknownGlobal :: Context -> String -> VM Object
vmUnknownGlobal ctx k =
  case contextReceiverMaybe ctx of
    Just receiver -> evalMessageSend False receiver (St.KeywordSelector "unknownGlobal:") [symbolObject k]
    _ -> vmError ("vmUnknownGlobal: " ++ show k)

{- | If a Return escapes we send an escapedBlock: message to the Object that the Block that Returned escaped from.
     For this purpose the Return object stores the Block that sent it.
     The Block can access the required Object from it's stored context.
     The Block that sent Return will be the current BlockContext.
     The Object that received the message that created a block will be the current MethodContext.
-}
vmEscapedBlock :: Maybe Object -> VM Object
vmEscapedBlock maybeBlock =
  case maybeBlock of
    Just block ->
      case block of
        Object _ (DataBlock _ context _) ->
          case contextReceiverMaybe context of
            Just receiver -> evalMessageSend False receiver (St.KeywordSelector "escapedBlock:") [block]
            Nothing -> vmError "escaped context: no receiver"
        _ -> vmError "escaped context: bad block"
    Nothing -> vmError "escaped context?"

-- * Resolve

-- | If a global does not exist, attempt to resolve it by loading a class file.
vmGlobalResolveMaybe :: Symbol -> VM (Maybe Object)
vmGlobalResolveMaybe key = do
  maybeResult <- vmGlobalLookupMaybe key
  case maybeResult of
    Just _ -> return maybeResult
    Nothing -> systemLoadClassMaybe key

-- | Resolve global, nil if not located.
vmGlobalResolveOrNil :: Symbol -> VM Object
vmGlobalResolveOrNil = fmap (fromMaybe nilObject) . vmGlobalResolveMaybe

-- | Resolve global, error if not located.
vmGlobalResolveOrError :: Symbol -> VM Object
vmGlobalResolveOrError key = vmGlobalResolveMaybe key >>= maybe (vmError ("vmGlobalResolve: " ++ key)) return

-- * Eval

-- | Lookup primitive in global dictionary, or error.
primitiveLookup :: (Symbol, Symbol) -> VM Primitive
primitiveLookup k = do
  case Map.lookup k primitiveDictionary of
    Just f -> return f
    Nothing -> vmError (printf "primitiveLookup: %s>>%s" (fst k) (snd k))

-- | Primitive>>invokeOnWith
evalPrimitiveInvokeOnWith :: Symbol -> Symbol -> Object -> [Object] -> VM Object
evalPrimitiveInvokeOnWith hld sig rcv arg = do
  f <- primitiveLookup (hld,sig)
  f rcv arg

-- | Evaluate StExpr in sequence.  If an StExpr evaluates to a Return Object it is returned and no further StExpr are evaluated.
evalExprSequence :: [StExpr] -> VM Object
evalExprSequence st =
  case st of
    [] -> error "evalExprSequence: empty sequence"
    [e] -> evalExpr e
    e0:eN -> do
      r <- evalExpr e0
      if isReturnObject r then return r else evalExprSequence eN

-- | An empty sequence returns nil, otherwise either a Return value or the value of the last StExpr is returned.
evalStatements :: [StExpr] -> VM Object
evalStatements st = if null st then return nilObject else evalExprSequence st

{- | evalBlock works by:
   1. extending the stored (block) context with a context frame
   2. saving the current (VM) context and replacing it with the extended stored context
   3. evaluating the block body and saving the result
   4. restoring the saved context
   5. returning the saved result
-}
evalBlock :: Object -> [Object] -> VM Object
evalBlock blockObject arguments = do
  let Object _ (DataBlock _ _ (Expr.Lambda _ _ _ blockStatements)) = blockObject
  extendedBlockContext <- contextAddBlockContext blockObject arguments
  currentContext <- vmContextReplace extendedBlockContext
  result <- evalStatements blockStatements
  _ <- vmContextReplace currentContext
  case result of
    Object _ (DataReturn pc maybeBlock _) ->
      if contextHasId pc currentContext
      then return result
      else vmEscapedBlock maybeBlock
    _ -> return result

{- | evalMethod is similar to evalBlock, except that methods:
   1. have a receiver which is stored and can be referenced as self or super
   2. store a context identifier in order to receive non-local returns
   2. don't have a stored (saved) context (they begin in the current environment, they are not closures)
   4. return self (the receiver) if there is no return statement

Return statements are allowed as the last statement of either a Method or a Block.
Returns in Blocks are non-local, they return to the blocks home context.
The home context is the method the block was defined in.

-}
evalMethod :: St.MethodDefinition -> [Symbol] -> St.Temporaries -> [StExpr] -> Object -> [Object] -> VM Object
evalMethod methodDefinition methodArguments methodTemporaries methodStatements receiver arguments = do
  -- printTrace ("evalMethod: " ++ St.methodSignature methodDefinition ++ " <= ") [receiver]
  let requiredArguments = length methodArguments
      providedArguments = length arguments
      arityError = printf "evalMethod: wrong number of arguments: %s %d" (St.methodSignature methodDefinition) providedArguments
  when (requiredArguments /= providedArguments) (vmError arityError)
  pc <- vmProgramCounterIncrement
  vmContextAdd =<< methodContextNode pc receiver (zip methodArguments arguments) methodTemporaries
  result <- evalStatements methodStatements
  _ <- vmContextDelete
  case result of
    (Object "Return" (DataReturn ctxId _ x)) -> if ctxId == pc then return x else return result
    _ -> return receiver

-- | Evaluate method, deferring to Primitive if required.
evalMethodOrPrimitive :: ObjectData -> Object -> [Object] -> VM Object
evalMethodOrPrimitive dat =
  let (DataMethod holder methodDefinition expr) = dat
      (Expr.Lambda _ methodArguments methodTemporaries methodStatements) = expr
  in if Som.somMethodIsPrimitive methodDefinition
     then evalPrimitiveInvokeOnWith holder (St.methodSignature methodDefinition)
     else evalMethod methodDefinition methodArguments methodTemporaries methodStatements

-- | Find method & evaluate, else send doesNotUnderstand message.
findAndEvalMethodOrPrimitive :: Object -> Object -> St.Selector -> [Object] -> VM Object
findAndEvalMethodOrPrimitive receiver methodReceiver selector arguments = do
  maybeMethod <- findMethodMaybe methodReceiver selector
  case maybeMethod of
    Nothing -> do
      argumentsArray <- arrayFromList arguments
      vmDoesNotUnderstand receiver (St.selectorIdentifier selector) argumentsArray
    Just (Object "Method" dat) -> evalMethodOrPrimitive dat receiver arguments
    _ -> vmError "findAndEvalMethodOrPrimitive"

-- | Look in the methods of the class, then in the superclass.
findMethodMaybe :: Object -> St.Selector -> VM (Maybe Object)
findMethodMaybe o sel =
  if isNil o
  then return Nothing
  else case classMethodsVec o of
         Just mth ->
          case Vector.find (\(Object _ (DataMethod _ m _)) -> sel == St.methodSelector m) mth of
            Just m -> return (Just m)
            Nothing -> classSuperclass o [] >>= \sc -> findMethodMaybe sc sel
         _ -> vmError "findMethodMaybe"

-- | Evaluate message send.
evalMessageSend :: Bool -> Object -> St.Selector -> [Object] -> VM Object
evalMessageSend isSuper receiver selector arguments = do
  receiverClass <- objectClass receiver []
  methodClass <- if isSuper
                 then classSuperclass receiverClass []
                 else return receiverClass
  findAndEvalMethodOrPrimitive receiver methodClass selector arguments

-- | Evaluate expression
evalExpr :: StExpr -> VM Object
evalExpr expr =
  case expr of
    Expr.Identifier x -> vmContextLookup (if x == "super" then "self" else x)
    Expr.Literal x -> literalObject x
    Expr.Assignment lhs rhs -> evalExpr rhs >>= vmContextAssign lhs
    Expr.Return x -> do
      result <- evalExpr x
      if isReturnObject result
      then return result
      else do pc <- vmContextId
              blk <- vmContextCurrentBlock
              returnObject pc blk result
    Expr.Send e (Expr.Message selector exprList) ->
      do receiver <- evalExpr e
         arguments <- mapM evalExpr exprList
         evalMessageSend (Expr.exprIsSuper e) receiver selector arguments
    Expr.Lambda _ld arg _tmp _stm -> do
      ctx <- vmContext
      pc <- vmProgramCounterIncrement
      return (Object ("Block" ++ show (length arg + 1)) (DataBlock pc ctx expr))
    Expr.Array exprList -> mapM evalExpr exprList >>= arrayFromList
    Expr.Begin exprList -> evalExprSequence exprList
    Expr.Init _ (St.Temporaries tmp) exprList -> vmContextAssignAllToNil tmp >> evalExprSequence exprList

-- | Parse string as a Smalltalk program, convert to Expr form, run evalExpr and return an Object.
evalString :: String -> VM Object
evalString txt = evalExpr (Expr.smalltalkProgramExpr (St.stParse St.smalltalkProgram txt))

deleteLeadingSpaces :: String -> String
deleteLeadingSpaces = dropWhile Data.Char.isSpace

{- | Run evalString given initial state and input text.
     If the text is empty (or whitespace only) return nil.
-}
vmEval :: VMState -> String -> IO (Either String Object, VMState)
vmEval vmState str =
  case deleteLeadingSpaces str of
    [] -> return (Right nilObject, vmState)
    txt -> State.runStateT (Except.runExceptT (evalString txt)) vmState

-- * Block Primitives

-- | Block1>>value
block1Value :: Primitive
block1Value rcv@(Object nm obj) arg = case (obj,arg) of
  (DataBlock _ _ _,[]) -> evalBlock rcv []
  _ -> prError ("Block1>>value " ++ nm)

-- | Block2>>value:
block2Value :: Primitive
block2Value rcv@(Object nm obj) arg = case (obj,arg) of
  (DataBlock _ _ _,[arg1]) -> evalBlock rcv [arg1]
  _ -> prError ("Block2>>value: " ++ nm)

-- | Block3>>value:with:
block3ValueWith :: Primitive
block3ValueWith rcv@(Object nm obj) arg = case (obj,arg) of
  (DataBlock _ _ _,[arg1,arg2]) -> evalBlock rcv [arg1,arg2]
  _ -> prError ("Block3>>value:with: " ++ nm)

-- * Class Primitives

{- | Get all variables of the indicated kind for the indicated class.
     This involves traversing the class hierachy to collect instance variables of all parent classes.
     The ordering places each subclasses instance variables after their superclasses.
     This value could be cached to avoid repeated lookups.
-}
classAllVariableNames :: (St.ClassDefinition -> [Symbol]) -> St.ClassDefinition -> VM [Symbol]
classAllVariableNames fn cd = do
  case St.superclassName cd of
    Just spName ->
      do res <- vmGlobalLookupMaybe spName
         case res of
           Just (Object _ (DataClass (spCd,_) _ _)) ->
             do spIv <- classAllVariableNames fn spCd
                return (spIv ++ fn cd)
           _ -> vmError "classAllVariableNames"
    Nothing -> return (fn cd)

-- | Class>>fields => Array[Symbol]
classFields :: Primitive
classFields (Object nm obj) arg = case (obj,arg) of
  (DataClass (cd,isMeta) _ _,[]) ->
    case isMeta of
      False -> do
        fld <- classAllVariableNames St.classInstanceVariableNames cd
        arrayFromList (map symbolObject fld)
      True -> do
        fld <- classAllVariableNames St.classVariableNames cd
        arrayFromList (map symbolObject fld)
  _ -> vmError ("Class>>fields " ++ nm)

{- | Create instance of class that is not defined primitively.
     Allocate reference for instance variables and initialize to nil.
     The instance variables of an object are:
         - the instance variables of it's class definition
         - all of the instance variables of all of it's superclasses.
-}
classNew :: Primitive
classNew (Object nm obj) arg =
  case (obj,arg) of
    (DataClass (cd,_) _ _,[]) -> do
      instVarNames <- classAllVariableNames St.classInstanceVariableNames cd
      tbl <- variablesTbl instVarNames
      pc <- vmProgramCounterIncrement
      return (Object (St.className cd) (DataUser pc tbl))
    _ -> vmError ("Class>>new " ++ nm)

{- | Class>>superclass => Class|nil

In Som the superclass of "Object" is "nil".
In Som the superclass of "Object class" is "Class".
This is the only case where a Metaclass has a superclass which is not a Metaclass.
For all other classes "C class superclass = C superclass class".

> Object superclass = nil                         "=> true"
> Object class superclass = Class                 "=> true"
> Nil class superclass = Nil superclass class     "=> true"
-}
classSuperclass :: Primitive
classSuperclass (Object nm obj) arg = case (obj,arg) of
  (DataClass (cd,isMeta) _ _,[]) ->
    if St.className cd == "Object"
    then if isMeta then vmGlobalLookupOrError "Class" else return nilObject
    else do
      sp <- maybe (return nilObject) vmGlobalResolveOrNil (St.superclassName cd)
      if isMeta then classMetaclass sp else return sp
  _ -> vmError ("Class>>superclass " ++ nm)

-- * Method Primitives

-- | Method>>holder (Method -> Class)
methodHolder :: Primitive
methodHolder (Object nm obj) arg = case (obj,arg) of
  (DataMethod holder _ _,[]) -> vmGlobalResolveOrError holder
  _ -> vmError ("Method>>holder " ++ nm)

-- | Method>>invokeOn:with: (Method -> Object -> [Object] -> Object)
methodInvokeOnWith :: Primitive
methodInvokeOnWith (Object nm obj) arg = case (nm,arg) of
  ("Method", [receiver, argumentsArray]) -> do
    arguments <- arrayElements argumentsArray
    evalMethodOrPrimitive obj receiver arguments
  _ -> objectListError (Object nm obj : arg) "Method>>invokeOn:with:" -- vmError ("Method>>invokeOn:with: " ++ nm)

-- * Object Primitives

{- | Class of class (Metaclass).
     If the Class object isMeta then return Metaclass, else set isMeta.
     Metaclass is a standard Som class, it is looked up in the global dictionary.
-}
classMetaclass :: Object -> VM Object
classMetaclass (Object _ obj) =
  case obj of
    DataClass (cd,isMeta) cVar mCache ->
      if isMeta
      then vmGlobalResolveOrError "Metaclass"
      else return (Object "Class" (DataClass (cd,True) cVar mCache))
    _ -> Except.throwError "classMetaclass"

{- | Object>>class (Object -> Class)

> 0 class = Integer                               "=> true"
> 0 class class = Integer class                   "=> true"
> 0 class class class = Metaclass                 "=> true"
> Metaclass class class = Metaclass               "=> true"
-}
objectClass :: Primitive
objectClass rcv@(Object nm obj) arg = case (obj,arg) of
  (DataClass _ _ _,[]) -> classMetaclass rcv
  (_,[]) -> vmGlobalLookupOrError nm
  _ -> vmError ("Object>>class " ++ nm)

-- | Object>>inspect (Object -> ())
objectInspect :: Primitive
objectInspect rcv arg = case arg of
  [] -> objectToInspector rcv >>= liftIO . putStrLn >> return rcv
  _ -> vmError "Object>>inspect"

{- | Object>>perform:inSuperclass: (Object -> Symbol -> Object -> Object)

> (1 perform: #class inSuperclass: Object) = Integer "=> true"
-}
objectPerformInSuperclass :: Primitive
objectPerformInSuperclass rcv arg = case arg of
  [Object "Symbol" (DataString sel),cl] -> do
    findAndEvalMethodOrPrimitive rcv cl (St.stParse St.quotedSelector ('#' : fromUnicodeString sel)) []
  _ -> objectError rcv "Object>>perform:"

{- | Object>>perform: (Object -> Symbol -> Object)

> (1 perform: #class) = Integer                   "=> true"
-}
objectPerform :: Primitive
objectPerform rcv arg = case arg of
  [sel] -> objectClass rcv [] >>= \cl -> objectPerformInSuperclass rcv [sel,cl]
  _ -> objectError rcv "Object>>perform:"

{- |Object>>perform:withArguments:inSuperclass:

> 1 perform: #+ withArguments: #(2) inSuperclass: Object
-}
objectPerformWithArgumentsInSuperclass :: Primitive
objectPerformWithArgumentsInSuperclass rcv arg = case arg of
  [Object "Symbol" (DataString sel),argumentsArray,cl] -> do
    arguments <- arrayElements argumentsArray
    findAndEvalMethodOrPrimitive rcv cl (St.stParse St.quotedSelector ('#' : fromUnicodeString sel)) arguments
  _ -> objectError rcv "Object>>perform:withArguments:inSuperclass:"

{- | Object>>perform:withArguments:

> (1 perform: #+ withArguments: #(2)) = 3         "=> true"
-}
objectPerformWithArguments :: Primitive
objectPerformWithArguments rcv arg = case arg of
  [sel,argArray] -> objectClass rcv [] >>= \cl -> objectPerformWithArgumentsInSuperclass rcv [sel,argArray,cl]
  _ -> objectError rcv "Object>>perform:withArguments:"

-- * Primitive Primitives

-- | Primitive>>invokeOn:with:.
primitiveInvokeOnWith :: Primitive
primitiveInvokeOnWith (Object nm obj) arg = case (obj,arg) of
  (DataPrimitive hld sig,[rcv,argumentsArray]) -> do
    arguments <- arrayElements argumentsArray
    evalPrimitiveInvokeOnWith hld sig rcv arguments
  _ -> vmError ("Primitive>>invokeOn:with: " ++ nm)

-- * System Primitives

{- | System>>load: (Symbol -> Class|nil)

> (system load: #Array) = Array                   "=> true"
> (system load: #UnknownClass) = nil              "=> true"
-}
systemLoad :: Primitive
systemLoad (Object nm obj) arg = case (obj,arg) of
  (DataSystem,[Object "Symbol" (DataString x)]) -> systemLoadClassOrNil (Text.unpack x)
  _ -> prError ("System>>load: " ++ nm)

-- | Load class or return nil.
systemLoadClassOrNil :: Symbol -> VM Object
systemLoadClassOrNil = fmap (fromMaybe nilObject) . systemLoadClassMaybe

-- | Load and return class (and required parent classes) if it exists.
systemLoadClassMaybe :: Symbol -> VM (Maybe Object)
systemLoadClassMaybe x = do
  c <- systemLoadAndAssignClassesAbove x
  case c of
    Nothing -> return Nothing
    Just _ -> vmGlobalLookupMaybe x

{- | Loads the named class and all of it's superclasses that are not already loaded.
     Assign each class in the global dictionary.
     Returns the last class loaded (ie. not necessarily the initial class requested).
     Halts when arriving at a class that is already loaded.
-}
systemLoadAndAssignClassesAbove :: Symbol -> VM (Maybe Object)
systemLoadAndAssignClassesAbove x = do
  existing <- vmGlobalLookupMaybe x
  case existing of
      Just _ -> return existing
      Nothing -> do
        maybeCd <- liftIO (Som.somLoadClassFile x)
        case maybeCd of
          Just cd -> do
            co <- classObject cd
            _ <- case St.superclassName cd of
                   Just sp -> systemLoadAndAssignClassesAbove sp
                   Nothing -> return (Just co)
            _ <- vmGlobalAssign (St.className cd) co
            return (Just co)
          _ -> return Nothing

-- * Primitive Dictionary

-- | Table of core primitives.
corePrimitiveTable :: PrimitiveTable
corePrimitiveTable =
  [
   (("Block1","value"),block1Value)
  ,(("Block2","value:"),block2Value)
  ,(("Block3","value:with:"),block3ValueWith)
  ,(("Class","fields"),classFields)
  ,(("Class","new"),classNew)
  ,(("Class","superclass"),classSuperclass)
  ,(("Method","holder"),methodHolder)
  ,(("Method","invokeOn:with:"),methodInvokeOnWith)
  ,(("Object","class"),objectClass)
  ,(("Object","inspect"),objectInspect)
  ,(("Object","perform:"),objectPerform)
  ,(("Object","perform:inSuperclass:"),objectPerformInSuperclass)
  ,(("Object","perform:withArguments:"),objectPerformWithArguments)
  ,(("Object","perform:withArguments:inSuperclass:"),objectPerformWithArgumentsInSuperclass)
  ,(("Primitive","invokeOn:with:"),primitiveInvokeOnWith)
  ,(("System","load:"),systemLoad)
  ]

-- | Dictionary of all Som primitives.
primitiveDictionary :: PrimitiveDictionary
primitiveDictionary = Map.fromList (corePrimitiveTable ++ primitiveTable)

-- * Tables

-- | Load the core Som classes and generate an object Table.
loadClassTable :: MonadIO m => FilePath -> m ObjectTable
loadClassTable somDirectory = do
  classLibrary <- liftIO (Som.somLoadClassList somDirectory Som.somStandardClassList)
  let classNames = map fst classLibrary
  classObjects <- mapM (classObject . snd) classLibrary
  return (zip classNames classObjects)

{- | Table of reserved identifiers: nil, true, false and system.
     These words are defined in System>>global.
-}
reservedIdentifiersTable :: ObjectTable
reservedIdentifiersTable =
  let f x = (x,reservedObject x)
  in map f (words "nil true false system")

-- | The initial global dictionary holds the class table and the reserved identifiers table.
initialGlobalDictionary :: MonadIO m => FilePath -> m Dict
initialGlobalDictionary somDirectory = do
  classTable <- loadClassTable somDirectory
  let compositeTable = concat [classTable, reservedIdentifiersTable]
  Env.dictRefFromList compositeTable

-- * Trace

printTrace :: MonadIO m => String -> [Object] -> m ()
printTrace msg o = liftIO (putStr msg) >> objectListPrint o >> return ()
