module Language.Smalltalk.Ansi.Ast.Helper where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Graph as Graph {- containers -}
import qualified Data.List.Split as Split {- split -}

import Language.Smalltalk.Ansi.Ast {- stsc3 -}

-- * 3.3.2 Class Definition

-- | Table relating subclass kind identifiers to Indexable values.
subclassKindTable :: [(LowercaseIdentifier, Indexable)]
subclassKindTable =
  [ ("subclass:", NonIndexable)
  , ("variableSubclass:", ObjectIndexable)
  , ("variableByteSubclass:", ByteIndexable)
  , ("variableWordSubclass:", WordIndexable)
  ]

subclassKindToIndexable :: LowercaseIdentifier -> Indexable
subclassKindToIndexable = maybe (error "subclassKindToIndexable") id . flip lookup subclassKindTable

indexableToSubclassKind :: Indexable -> LowercaseIdentifier
indexableToSubclassKind = maybe (error "indexableToSubclassKind") id . flip lookup (map (\(p, q) -> (q, p)) subclassKindTable)

instanceStateToSubclassKind :: InstanceState -> LowercaseIdentifier
instanceStateToSubclassKind (InstanceState ix _) = indexableToSubclassKind ix

noInstanceState :: InstanceState
noInstanceState = InstanceState NonIndexable []

-- | A Metaclass name is the class name with ' class' appended.
metaclassName :: UppercaseIdentifier -> UppercaseIdentifier
metaclassName x = x ++ " class"

isMetaclassName :: UppercaseIdentifier -> Bool
isMetaclassName x = " class" `isSuffixOf` x

{- | Remove the ' class' suffix from a Metaclass name.

>>> metaclassNameClassName "Array class"
"Array"
-}
metaclassNameClassName :: UppercaseIdentifier -> UppercaseIdentifier
metaclassNameClassName x =
  if isMetaclassName x
    then take (length x - 6) x
    else error ("metaclassNameClassName: not Metaclass: " ++ x)

{- | The name of the meta class, ie. the class name with a class suffix.
     Metaclasses do not have separate ClassDefinitions.
     In Smalltalk the rule is that the class of a metaclass is "Metaclass".
     This includes the class of "Metaclass class", forming a cycle.
     This does not arise here since "Metaclass class" does not have a ClassDefinition.
-}
classMetaclassName :: ClassDefinition -> UppercaseIdentifier
classMetaclassName = metaclassName . className

{- | "Smalltalk implementations have traditionally open-coded certain
messages including those with the following selectors."
-}
restrictedSelectors :: [LowercaseIdentifier]
restrictedSelectors =
  [ "ifTrue:"
  , "ifTrue:ifFalse:"
  , "ifFalse:"
  , "ifFalse:ifTrue:"
  , "to:do:"
  , "to:by:do:"
  , "and:"
  , "or:"
  , "=="
  , "timesRepeat:"
  , "basicAt:"
  , "basicAt:put:"
  , "basicSize"
  , "basicNew:"
  ]

-- | List of instance method categories in alphabetical order.
classInstanceMethodCategories :: ClassDefinition -> [String]
classInstanceMethodCategories = nub . sort . map methodCategoryRequired . instanceMethods

-- | All class method categories in alphabetical order.
classClassMethodCategories :: ClassDefinition -> [String]
classClassMethodCategories = nub . sort . map methodCategoryRequired . classMethods

-- | Does the class have a class side "initialize" method?
classHasClassInitializeMethod :: ClassDefinition -> Bool
classHasClassInitializeMethod = elem "initialize" . map (selectorIdentifier . patternSelector . methodPattern) . classMethods

classCategoryOrError :: ClassDefinition -> String
classCategoryOrError = fromMaybe (error "classCategory") . classCategory

{- | Split category string into two parts at hyphen.

>>> map categoryParts (words "Kernel-Numbers Sound-Sc3 Broken")
[("Kernel","Numbers"),("Sound","Sc3"),("Unknown","Broken")]
-}
categoryParts :: String -> ClassCategoryParts
categoryParts cat =
  case elemIndex '-' cat of
    Just ix -> (take ix cat, drop (ix + 1) cat)
    _ -> ("Unknown", cat)

classCategoryParts :: ClassDefinition -> Maybe ClassCategoryParts
classCategoryParts = fmap categoryParts . classCategory

classCategoryPartsOrError :: ClassDefinition -> ClassCategoryParts
classCategoryPartsOrError = categoryParts . classCategoryOrError

classDefinitionSetCategory :: Maybe String -> ClassDefinition -> ClassDefinition
classDefinitionSetCategory cat cd = cd {classCategory = cat}

-- | Apply f to both class and instance methods.
classDefinitionEditMethods :: ([MethodDefinition] -> [MethodDefinition]) -> ClassDefinition -> ClassDefinition
classDefinitionEditMethods f cd =
  let im = instanceMethods cd
      cm = classMethods cd
  in cd {instanceMethods = f im, classMethods = f cm}

-- | Sort methods by name.
classDefinitionSortMethods :: ClassDefinition -> ClassDefinition
classDefinitionSortMethods = classDefinitionEditMethods (sortOn methodName)

-- | Apply source editor to all methods.
classDefinitionEditMethodSources :: (String -> String) -> ClassDefinition -> ClassDefinition
classDefinitionEditMethodSources f = classDefinitionEditMethods (map (methodDefinitionEditSource f))

-- | Generate a class definition from a list of methods definitions.
classDefinitionFromMethods :: (UppercaseIdentifier, Maybe String, Maybe String) -> [MethodDefinition] -> ClassDefinition
classDefinitionFromMethods (nm, cat, cmt) mth =
  let (cm, im) = partition isClassMethod mth
  in if nub (map methodClassName mth) == [nm]
      then ClassDefinition nm Nothing noInstanceState [] [] [] im cm Nothing cat cmt
      else error "classDefinitionFromMethods: wrong method list?"

classDefinitionHasMethod :: ClassDefinition -> MethodDescriptor -> Bool
classDefinitionHasMethod cd for = any (isMethodFor for) (classDefinitionMethods cd)

-- | Checks that methods do not already exist at class, c.f. classDefinitionReplaceMethods
classDefinitionExtendWithMethods :: ClassDefinition -> [MethodDefinition] -> ClassDefinition
classDefinitionExtendWithMethods cd mth =
  let nm = className cd
      (cm, im) = partition isClassMethod mth
      notFor = filter (/= nm) (map methodClassName mth)
      alreadyExist = filter (classDefinitionHasMethod cd) (map methodDescriptor mth)
  in if null mth
      then cd
      else
        if null notFor && null alreadyExist
          then cd {instanceMethods = instanceMethods cd ++ im, classMethods = classMethods cd ++ cm}
          else
            error
              ( concat
                  [ "classDefinitionExtendWithMethods: wrong method list for: "
                  , nm
                  , " of: "
                  , unwords (map methodSignature mth)
                  , " because: " ++ show (notFor, alreadyExist)
                  ]
              )

-- | Checks that all methods already exist at class, c.f. classDefinitionExtendWithMethods
classDefinitionReplaceMethods :: ClassDefinition -> [MethodDefinition] -> ClassDefinition
classDefinitionReplaceMethods cd mth =
  let nm = className cd
      notFor = filter (/= nm) (map methodClassName mth)
      dontExist = filter (not . classDefinitionHasMethod cd) (map methodDescriptor mth)
      mthTbl = map (\m -> (methodDescriptor m, m)) mth
      replaceMethod m = fromMaybe m (lookup (methodDescriptor m) mthTbl)
  in if null mth
      then cd
      else
        if null notFor && null dontExist
          then cd {instanceMethods = map replaceMethod (instanceMethods cd), classMethods = map replaceMethod (classMethods cd)}
          else
            error
              ( concat
                  [ "classDefinitionReplaceMethods: wrong method list for: "
                  , nm
                  , " of: "
                  , unwords (map methodSignature mth)
                  , " because: " ++ show (notFor, dontExist)
                  ]
              )

classDefinitionMethods :: ClassDefinition -> [MethodDefinition]
classDefinitionMethods cd = instanceMethods cd ++ classMethods cd

-- | A class definition that extends or modifies an existing class should have its superclass set to itself.
classDefinitionIsExtensionOrModification :: ClassDefinition -> Bool
classDefinitionIsExtensionOrModification cd = Just (className cd) == superclassName cd

-- | Graph where each class definition is connected to it's superclass, iff that class is also in the list of definitions.
classDefinitionsInheritanceGraph :: [ClassDefinition] -> ClassDefinitionGraph
classDefinitionsInheritanceGraph l =
  let nd = map className l
      e cd = case superclassName cd of
        Nothing -> []
        Just sc -> if sc `elem` nd then [sc] else []
  in Graph.graphFromEdges (zip3 l nd (map e l))

classDefinitionGraphSort :: ClassDefinitionGraph -> [ClassDefinition]
classDefinitionGraphSort (g, f, _) = reverse (map ((\(cd, _, _) -> cd) . f) (Graph.topSort g)) -- 0.6.4 has reverseTopSort

-- * 3.4.2


isMethodFor :: MethodDescriptor -> MethodDefinition -> Bool
isMethodFor (classSide, selector) m = (classSide == isClassMethod m) && (selector == methodSelector m)

-- | Class name method is for, without the " class" suffix for class methods.
methodClassName :: MethodDefinition -> UppercaseIdentifier
methodClassName m =
  case methodClass m of
    (nm, False) -> nm
    (nm, True) -> metaclassNameClassName nm

isClassMethod :: MethodDefinition -> Bool
isClassMethod = snd . methodClass

-- | The default method category.  This applies both to methods without any category and to methods with an empty category string.
methodDefaultCategory :: String
methodDefaultCategory = "as yet unclassified"

-- | Method category or the default.
methodCategoryRequired :: MethodDefinition -> String
methodCategoryRequired m =
  case methodCategory m of
    Nothing -> methodDefaultCategory
    Just cat -> if null cat then methodDefaultCategory else cat

{- | Does MethodDefinition end with a Return (local).
     Note this does not look to see if the method includes any non-local returns.
-}
methodDefinitionEndsWithReturn :: MethodDefinition -> Bool
methodDefinitionEndsWithReturn = maybe False statementsEndsWithReturn . methodStatements

-- | Get label of primitive, if there is one.
methodDefinitionPrimitiveLabel :: MethodDefinition -> Maybe Literal
methodDefinitionPrimitiveLabel = fmap primitiveLabel . methodPrimitive

-- | Predicate to examine a MethodDefinition and decide if it is a primitive.
methodDefinitionHasPrimitive :: MethodDefinition -> Bool
methodDefinitionHasPrimitive = isJust . methodDefinitionPrimitiveLabel

-- | Get integer code of primitive, if there is one.
methodDefinitionPrimitiveCode :: MethodDefinition -> Maybe Integer
methodDefinitionPrimitiveCode =
  let f = maybe (error "methodDefinitionPrimitiveCode: not integer?") id . literalInteger
  in fmap f . methodDefinitionPrimitiveLabel

methodDefinitionEditSource :: (String -> String) -> MethodDefinition -> MethodDefinition
methodDefinitionEditSource f md =
  let src = methodSource md
  in md {methodSource = fmap f src}

methodDefinitionArguments :: MethodDefinition -> [LowercaseIdentifier]
methodDefinitionArguments = patternArguments . methodPattern

methodDefinitionTemporaries :: MethodDefinition -> [LowercaseIdentifier]
methodDefinitionTemporaries = maybe [] temporariesIdentifiers . methodTemporaries

-- | Answer names that are given both as arguments and temporaries.
methodDefinitionDuplicateTemporaries :: MethodDefinition -> [LowercaseIdentifier]
methodDefinitionDuplicateTemporaries m =
  let a = methodDefinitionArguments m
      t = methodDefinitionTemporaries m
  in (a ++ t) \\ nub (a ++ t)

-- * 3.4.2

{- | Derive method selector from Pattern.
     Return is either Identifier or BinarySelector (both Strings).

>>> let p = stParse messagePattern
>>> patternSelector (p "midicps")
UnarySelector "midicps"

>>> patternSelector (p "| aBoolean")
BinarySelector "|"

>>> patternSelector (p "+ aNumber")
BinarySelector "+"

>>> patternSelector (p "new: x")
KeywordSelector "new:" 1

>>> patternSelector (p "freq: f phase: p")
KeywordSelector "freq:phase:" 2
-}
patternSelector :: Pattern -> Selector
patternSelector pat =
  case pat of
    UnaryPattern u -> UnarySelector u
    BinaryPattern b _ -> BinarySelector b
    KeywordPattern kp -> KeywordSelector (concatMap fst kp) (length kp)

{- | Derive argument list from Pattern.

>>> map patternArguments [UnaryPattern "x",BinaryPattern "+" "x",KeywordPattern [("x:","p"),("y:","q")]]
[[],["x"],["p","q"]]
-}
patternArguments :: Pattern -> [LowercaseIdentifier]
patternArguments pat =
  case pat of
    UnaryPattern _ -> []
    BinaryPattern _ p -> [p]
    KeywordPattern kp -> map snd kp

-- | Method argument list.
methodArguments :: MethodDefinition -> [LowercaseIdentifier]
methodArguments = patternArguments . methodPattern

-- | Method selector.
methodSelector :: MethodDefinition -> Selector
methodSelector = patternSelector . methodPattern

methodDescriptor :: MethodDefinition -> MethodDescriptor
methodDescriptor m = (isClassMethod m, methodSelector m)

-- | Untyped identifier for method selector.
methodSignature :: MethodDefinition -> LowercaseIdentifier
methodSignature = selectorIdentifier . methodSelector

-- | Calculate MethodName from MethodDefinition.
methodName :: MethodDefinition -> MethodName
methodName m = (fst (methodClass m), methodSignature m)

-- | Method name in traditional Smalltalk form, ie. ClassName>>methodSignature.
methodNameIdentifier :: MethodName -> String
methodNameIdentifier (cl, sg) = cl ++ ">>" ++ sg

-- | Number of temporaries.
temporariesLength :: Temporaries -> Int
temporariesLength (Temporaries x) = length x

-- | Empty Temporaries list.
emptyTemporaries :: Temporaries
emptyTemporaries = Temporaries []

-- * 3.4.3

{- | Construct a standard class initiliaze statement, i.e. "ClassName initialize"

> standardClassInitializerDefinition "ClassName"
-}
standardClassInitializerDefinition :: UppercaseIdentifier -> InitializerDefinition
standardClassInitializerDefinition nm =
  let e = ExprBasic (simpleUnaryMessageSend (PrimaryIdentifier nm) "initialize")
  in InitializerDefinition Nothing Nothing (Just (StatementsExpression e Nothing))

-- | Set comment field.
initializerDefinitionSetComment :: Comment -> InitializerDefinition -> InitializerDefinition
initializerDefinitionSetComment c (InitializerDefinition _ t s) = InitializerDefinition (Just c) t s

-- * 3.4.4

{- | Does BlockBody end with a Return (non-local).
     Note this does not look to see if the block contains any nested non-local returns.
-}
blockBodyEndsWithReturn :: BlockBody -> Bool
blockBodyEndsWithReturn = maybe False statementsEndsWithReturn . blockStatements

blockBodyArguments :: BlockBody -> [LowercaseIdentifier]
blockBodyArguments = fromMaybe [] . blockArguments

blockBodyTemporaries :: BlockBody -> [LowercaseIdentifier]
blockBodyTemporaries = maybe [] temporariesIdentifiers . blockTemporaries

-- | Names that are given both as block arguments and as temporaries.
blockBodyDuplicateTemporaries :: BlockBody -> [LowercaseIdentifier]
blockBodyDuplicateTemporaries m =
  let a = blockBodyArguments m
      t = blockBodyTemporaries m
  in (a ++ t) \\ nub (a ++ t)

-- * 3.4.5

-- | Unfold Statements value into a sequence of expressions and an optional return expression.
statementsUnfold :: Statements -> ([Expression], Maybe Expression)
statementsUnfold =
  let f lst stm =
        case stm of
          StatementsReturn (ReturnStatement ret) -> (reverse lst, Just ret)
          StatementsExpression expr Nothing -> (reverse (expr : lst), Nothing)
          StatementsExpression expr (Just stm') -> f (expr : lst) stm'
  in f []

-- | Does Statements end with a Return?
statementsEndsWithReturn :: Statements -> Bool
statementsEndsWithReturn = isJust . snd . statementsUnfold

-- | Prepend a list of expressions, as statements, to an existing statement.
expressionSequenceToStatements :: Maybe Statements -> [Expression] -> Statements
expressionSequenceToStatements stm =
  let f e =
        case e of
          [] -> error "expressionSequenceToStatements"
          [e0] -> StatementsExpression e0 stm
          e0 : eN -> StatementsExpression e0 (Just (f eN))
  in f

expressionCase :: (Assignment -> t) -> (BasicExpression -> t) -> Expression -> t
expressionCase f g e =
  case e of
    ExprAssignment x -> f x
    ExprBasic x -> g x

{- | Construct simple unary message send to primary receiver

> simpleUnaryMessageSend (PrimaryIdentifier "Array") "new"
-}
simpleUnaryMessageSend :: Primary -> LowercaseIdentifier -> BasicExpression
simpleUnaryMessageSend rcv msg =
  BasicExpression rcv (Just (MessagesUnary [UnaryMessage msg] Nothing Nothing)) Nothing

{- | If the expression consists only of a primary, return that.
     If the expression has messages make a PrimaryExpression node.
-}
basicExpressionToPrimary :: BasicExpression -> Primary
basicExpressionToPrimary e =
  case e of
    BasicExpression p Nothing Nothing -> p
    _ -> PrimaryExpression (ExprBasic e)

unaryMessageSelector :: UnaryMessage -> Selector
unaryMessageSelector (UnaryMessage u) = UnarySelector u

binaryMessageSelector :: BinaryMessage -> Selector
binaryMessageSelector (BinaryMessage b _) = BinarySelector b

-- | Keyword selector from KeywordMessage.
keywordMessageSelector :: KeywordMessage -> Selector
keywordMessageSelector (KeywordMessage l) = KeywordSelector (concatMap fst l) (length l)

-- * 3.4.6

literalInteger :: Literal -> Maybe Integer
literalInteger lit =
  case lit of
    NumberLiteral (Int k) -> Just k
    _ -> Nothing

integerLiteral :: Integer -> Literal
integerLiteral = NumberLiteral . Int

-- | Case analysis of Number type.
numberCase :: (Integer -> t) -> (Double -> t) -> Number -> t
numberCase f1 f2 n =
  case n of
    Int x -> f1 x
    Float x -> f2 x

numberFloat :: Number -> Double
numberFloat = numberCase fromIntegral id

-- * 3.4.7 Reserved Identifiers

{- | These are the reserved identifiers.
     self and super are not of the same kind with regards to unquoted symbols in literal arrays.
-}
stReservedIdentifiers :: [String]
stReservedIdentifiers = words "nil true false self super"

-- * 3.5.5

-- | The list of characters that form operator names.
binaryCharacterSet :: [Char]
binaryCharacterSet = "!%&*+,/<=>?@\\~|-"

{- | Is an identifier a binary operator?

>>> map isBinaryIdentifier (words "+ - * / % @ ** | &")
[True,True,True,True,True,True,True,True,True]
-}
isBinaryIdentifier :: Identifier -> Bool
isBinaryIdentifier = all (`elem` binaryCharacterSet)

-- * 3.5.8

{- | Remove quote characters from QuotedString.

>>> unquoteQuotedString "'string'"
"string"

>>> unquoteQuotedString "'instance creation'"
"instance creation"
-}
unquoteQuotedString :: QuotedString -> String
unquoteQuotedString x = take (length x - 2) (drop 1 x)

-- * 3.5.10

-- | The keyword selector stores its arity, the number of message parameters.
asKeywordSelector :: LowercaseIdentifier -> Selector
asKeywordSelector x = KeywordSelector x (length (filter (== ':') x))

-- | Is Selector an infix binary operator.
isBinarySelector :: Selector -> Bool
isBinarySelector s =
  case s of
    BinarySelector _ -> True
    _ -> False

-- | Identifier of Selector
selectorIdentifier :: Selector -> Identifier
selectorIdentifier s =
  case s of
    UnarySelector x -> x
    BinarySelector x -> x
    KeywordSelector x _ -> x

{- | Split KeywordSelector into it's components.

>>> keywordSelectorElements "freq:"
["freq:"]

>>> keywordSelectorElements "freq:phase:"
["freq:","phase:"]

>>> keywordSelectorElements ""
[]
-}
keywordSelectorElements :: LowercaseIdentifier -> [LowercaseIdentifier]
keywordSelectorElements =
  takeWhile (not . null)
    . (Split.split . Split.keepDelimsR . Split.onSublist) ":"

{- | Determine arity of selector.
The arity does not include the receiver.

>>> map selectorArity [UnarySelector "abs",BinarySelector "+",KeywordSelector "at:put:" 2]
[0,1,2]
-}
selectorArity :: Selector -> Int
selectorArity s =
  case s of
    UnarySelector _ -> 0
    BinarySelector _ -> 1
    KeywordSelector _ x -> x

-- * Primitive

-- | Primitive with integer label.
primitiveOf :: Integer -> Primitive
primitiveOf = Primitive . integerLiteral
