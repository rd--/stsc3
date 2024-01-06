{- | Ast and parser for a subset of ANSI Smalltalk.
     The file is organised according to the report.
     Numbers refer to sections of the report.
     <https://wiki.squeak.org/squeak/uploads/172/standard_v1_9-indexed.pdf>

SmalltalkProgram ProgramElement (3.3.1)
ClassDefinition (3.3.2)
GlobalDefinition (3.3.3)
ProgramInitializerDefinition (3.3.5)
MethodDefinition, Temporaries (3.4.2)
InitializerDefinition (3.4.3)
BlockBody (3.4.4)
Statements ReturnStatement Expression Assignment BasicExpression Primary Messages (3.4.5)
Literal (3.4.6)
-}
module Language.Smalltalk.Ansi where

import Control.Monad {- base -}
import Data.Char {- base -}
import qualified Data.Functor.Identity as Identity {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Numeric {- base -}

import qualified Data.Graph as Graph {- containers -}
import qualified Data.List.Split as Split {- split -}

import qualified Text.Parsec as P {- parsec -}
import qualified Text.Parsec.Language as Language {- parsec -}
import qualified Text.Parsec.String as String {- parsec -}
import qualified Text.Parsec.Token as Token {- parsec -}

-- | A 'Char' parser with no user state.
type P a = String.GenParser Char () a

-- | Run p then q, returning result of p.
(>>~) :: Monad m => m t -> m u -> m t
p >>~ q = p >>= \x -> q >> return x

-- | /p/ consuming any trailing separators.
lexeme :: P t -> P t
lexeme = Token.lexeme stLexer

-- | Smalltalk language definition (for token parser)
stLanguageDef :: Language.GenLanguageDef String () Identity.Identity
stLanguageDef =
  Token.LanguageDef
    { Token.commentStart = "\""
    , Token.commentEnd = "\""
    , Token.commentLine = "" -- NIL
    , Token.nestedComments = False
    , Token.identStart = letter
    , Token.identLetter = letter P.<|> P.digit
    , Token.opStart = binaryCharacter
    , Token.opLetter = binaryCharacter
    , Token.reservedNames = stReservedIdentifiers
    , Token.reservedOpNames = []
    , Token.caseSensitive = True
    }

-- | Lexer
stLexer :: Token.GenTokenParser String () Identity.Identity
stLexer = Token.makeTokenParser stLanguageDef

-- | Run parser and return either an error string or an answer.
stParseEither :: P t -> String -> Either String t
stParseEither p = either (\m -> Left ("stParse: " ++ show m)) Right . P.parse p ""

-- | Run parser and report any error.  Does not delete leading spaces.
stParse :: P t -> String -> t
stParse p = either (\e -> error e) id . stParseEither p

-- | Run parser and report maybe give answer.
stParseMaybe :: P t -> String -> Maybe t
stParseMaybe p = either (\_ -> Nothing) Just . P.parse p ""

-- | Delete leading spaces and run stParse.
stParseInitial :: P t -> String -> t
stParseInitial p =
  let deleteLeadingSpaces = dropWhile isSpace
  in stParse p . deleteLeadingSpaces

-- * 3.3 Smalltalk Abstract Program Grammar

-- * 3.3.1 Program Definition

-- | Sequence of program elements.
data SmalltalkProgram = SmalltalkProgram {programElements :: [ProgramElement]}
  deriving (Eq, Show)

{- | <<Smalltalk program>> ::= <<program element>>+ <<initialization ordering>>

>>> p = stParse smalltalkProgram
>>> p "" == SmalltalkProgram {programElements = []}
True

> p "self ifNil:" -- ? error
-}
smalltalkProgram :: P SmalltalkProgram
smalltalkProgram = do
  P.optional separator
  fmap SmalltalkProgram (P.many1 nonEmptyProgramElement P.<|> (P.eof >> return []))

{-
import Control.Monad.Loops {- monad-loops -}

{- | P.many1 cannot accept a parser that can match an empty string, which programElement can.
     This attempts parse ProgramElements until eof.
     However in the case of a syntax error this will not halt.
-}
programElementSequence :: P [ProgramElement]
programElementSequence = Control.Monad.Loops.unfoldM maybeProgramElement

maybeProgramElement :: P (Maybe ProgramElement)
maybeProgramElement = (P.eof >> return Nothing) P.<|> fmap Just programElement
-}

-- | 3.3.1
data ProgramElement
  = ProgramGlobal GlobalDefinition
  | ProgramInitializer ProgramInitializerDefinition
  deriving (Eq, Show)

{- | <<program element>> ::= <<class definition>>
                           | <<global definition>>
                           | <<pool definition>>
                           | <<program initializer definition>>
-}
programElement :: P ProgramElement
programElement =
  P.choice
    [ fmap ProgramInitializer programInitializerDefinition
    , fmap ProgramGlobal globalDefinition
    ]

nonEmptyProgramElement :: P ProgramElement
nonEmptyProgramElement =
  P.choice
    [ fmap ProgramInitializer nonEmptyProgramInitializerDefinition
    , fmap ProgramGlobal globalDefinition
    ]

-- * 3.3.2 Class Definition

data Indexable = ByteIndexable | NonIndexable | ObjectIndexable | WordIndexable deriving (Eq, Show)

data InstanceState = InstanceState Indexable [Identifier] deriving (Eq, Show)

{- | Smalltalk has conventions regarding identifier case, but not strict rules.
In .stc it would be nice to allow p.Q to mean Q(p), ie. distinguishing between q and Q.
The types here are used to indicate where there is an ordinary case for a particular identifier.
-}
type LowercaseIdentifier = Identifier

type UppercaseIdentifier = Identifier

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

{- | A class definition defines the behavior and encapsulated state of objects.

superclassName is empty (Nothing) if the class is the base class (i.e. Object or ProtoObject) else it is the name of the super class.
An empty superclassName does not indicate that a "default" superclass should be selected.
-}
data ClassDefinition = ClassDefinition
  { className :: UppercaseIdentifier
  , superclassName :: Maybe UppercaseIdentifier
  , instanceState :: InstanceState
  , classInstanceVariableNames :: [LowercaseIdentifier]
  , classVariableNames :: [UppercaseIdentifier]
  , importedPoolNames :: [UppercaseIdentifier]
  , instanceMethods :: [MethodDefinition]
  , classMethods :: [MethodDefinition]
  , classInitializer :: Maybe InitializerDefinition
  , classCategory :: Maybe String -- Meta-data
  , classComment :: Maybe String -- Meta-data
  }
  deriving (Eq, Show)

-- | A class library is a collection of class definitions.
type ClassLibrary = [ClassDefinition]

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

-- | (Major, Minor)
type ClassCategoryParts = (String, String)

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

type ClassDefinitionGraph =
  ( Graph.Graph
  , Graph.Vertex -> (ClassDefinition, UppercaseIdentifier, [UppercaseIdentifier])
  , UppercaseIdentifier -> Maybe Graph.Vertex
  )

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

-- * 3.3.3 Global Variable Definition

-- | 3.3.3
data GlobalDefinition
  = GlobalDefinition GlobalName (Maybe VariableInitializer)
  deriving (Eq, Show)

-- | <<global name>> ::= identifier
type GlobalName = UppercaseIdentifier

-- | <<variable initializer>> ::= <initializer definition>
type VariableInitializer = InitializerDefinition

{- | <<global definition>> ::= [<<constant designator>>] <<global name>> [<<variable initializer>>]

>>> p = stParse globalDefinition
>>> p "g"
GlobalDefinition "g" (Just (InitializerDefinition Nothing Nothing Nothing))

> p "g g := 0"
-}
globalDefinition :: P GlobalDefinition
globalDefinition = do
  -- constant designator
  n <- identifier
  v <- P.optionMaybe initializerDefinition
  return (GlobalDefinition n v)

-- * 3.3.5 Program Initializer Definition

-- | 3.3.5
type ProgramInitializerDefinition = InitializerDefinition

{- | <<program initializer definition >> ::= <initializer definition>

>>> p = stParse programInitializerDefinition

> p "|t| t + 1"
> p "\"x\" |t| t + 1"

>>> p ""
InitializerDefinition Nothing Nothing Nothing
-}
programInitializerDefinition :: P ProgramInitializerDefinition
programInitializerDefinition = initializerDefinition

-- | A variant that does will not match the empty string.
nonEmptyProgramInitializerDefinition :: P ProgramInitializerDefinition
nonEmptyProgramInitializerDefinition = nonEmptyInitializerDefinition

-- * 3.4.2

-- | Method category (Meta-data)
type MethodCategory = String

{- | 3.4.2

     It is a convention that method comments, if they exist, are written directly after the pattern.
     The parser presently does not store Method comments, however there is a placeholder field.
     In order to store comments:
       - stLanguageDef should not define comment start and end
       - there should be a comment preserving lexeme form, lexemeWithMaybeComment
       - methodPattern should be such a lexeme
     There is a field for storing the method source text.
     The methodClass field indicates which class the method is held by.
     It will be ("Name", False) for instance methods and ("Name class", True) for class methods.
-}
data MethodDefinition = MethodDefinition
  { methodClass :: (UppercaseIdentifier, Bool)
  -- ^ Holder
  , methodCategory :: Maybe MethodCategory
  -- ^ Meta-data
  , methodPattern :: Pattern
  , methodTemporaries :: Maybe Temporaries
  , methodStatements :: Maybe Statements
  , methodPrimitive :: Maybe Primitive
  -- ^ Non-ansi
  , methodComment :: Maybe String
  , methodSource :: Maybe String
  }
  deriving (Eq, Show)

-- | (isClassSide, selector)
type MethodDescriptor = (Bool, Selector)

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

methodDefinitionDuplicateTemporaries :: MethodDefinition -> [LowercaseIdentifier]
methodDefinitionDuplicateTemporaries m =
  let a = methodDefinitionArguments m
      t = methodDefinitionTemporaries m
  in (a ++ t) \\ nub (a ++ t)

duplicateNamesError :: [LowercaseIdentifier] -> P ()
duplicateNamesError dup = when (not (null dup)) (P.unexpected ("Name already used: " ++ show dup))

{- | <method definition> ::= <message pattern> [<temporaries>] [<statements>]

> p = stParse (methodDefinition Nothing ("", False))
> p "p"
> p "p q"
> p "p ^q"
> p "p q. ^r"
> p "p |t|"
> p "p |t| r"
> p "p |t| r. ^s"
> p "p: q"
> p "p: q r"
> p "p: q ^r"
> p "p: q r. ^s"
> p "printElementsOn: aStream aStream nextPut: $(."
> p "* anObject ^self shallowCopy *= anObject"
> p "p \"c\" ^q"
> p "p |tmp| <primitive: 0> self continueAfterPrimitive"
> p "p |tmp| <primitive: 'stringAsLabel'> ^nil"
> p "p |tmp| <primitive: identifierAsLabel> ^nil"
> p "p <primitive: 0> |tmp| self primitiveFailed" -- this should fail, instead it discards statements!
> p "p: q | q | ^q" -- duplicate name error
-}
methodDefinition :: Maybe String -> (UppercaseIdentifier, Bool) -> P MethodDefinition
methodDefinition src cl = do
  pat <- messagePattern -- messagePattern is a token and consumes trailing comments
  cmt <- P.optionMaybe comment -- this is always Nothing
  tmp <- P.optionMaybe temporaries
  prm <- P.optionMaybe primitive -- primitive comes after temporaries
  stm <- P.optionMaybe statements
  let def = MethodDefinition cl Nothing pat tmp stm prm cmt src
      dup = methodDefinitionDuplicateTemporaries def
  duplicateNamesError dup
  return def

data Pattern
  = UnaryPattern LowercaseIdentifier
  | BinaryPattern BinaryIdentifier LowercaseIdentifier
  | KeywordPattern [(Keyword, LowercaseIdentifier)]
  deriving (Eq, Show)

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

-- | A method name is a (className,methodSignature)
type MethodName = (UppercaseIdentifier, LowercaseIdentifier)

-- | Calculate MethodName from MethodDefinition.
methodName :: MethodDefinition -> MethodName
methodName m = (fst (methodClass m), methodSignature m)

-- | Method name in traditional Smalltalk form, ie. ClassName>>methodSignature.
methodNameIdentifier :: MethodName -> String
methodNameIdentifier (cl, sg) = cl ++ ">>" ++ sg

{- | <message pattern> ::= <unary pattern> | <binary pattern> | <keyword pattern>

>>> let p = stParse messagePattern

>>> p "p"
UnaryPattern "p"

>>> p "+p" == p "+ p"
True

>>> p "k1:p1" == p "k1: p1"
True

>>> p "k1:p1 k2:p2" == p "k1: p1 k2: p2"
True

>>> p "k: v x" == p "k:v"
True
-}
messagePattern :: P Pattern
messagePattern =
  P.choice
    [ P.try keywordPattern
    , P.try binaryPattern
    , unaryPattern
    ]
    P.<?> "messagePattern"

{- | <unary pattern> ::= unarySelector

>>> stParse unaryPattern "p"
UnaryPattern "p"

> stParse unaryPattern "p:" -- error
-}
unaryPattern :: P Pattern
unaryPattern = fmap (UnaryPattern . selectorIdentifier) unarySelector -- lexeme

{- | <binary pattern> ::= binarySelector <method argument>

>>> stParse binaryPattern "+p" == stParse binaryPattern "+ p"
True
-}
binaryPattern :: P Pattern
binaryPattern = do
  BinarySelector sel <- binarySelector
  arg <- identifier
  return (BinaryPattern sel arg)

{- | <keyword pattern> ::= (keyword <method argument>)+

>>> stParse keywordPattern "k: p"
KeywordPattern [("k:","p")]

>>> stParse keywordPattern "k1: p1 k2: p2"
KeywordPattern [("k1:","p1"),("k2:","p2")]

>>> stParse keywordPattern "k1: p1 ..."
KeywordPattern [("k1:","p1")]
-}
keywordPattern :: P Pattern
keywordPattern = do
  let f = do
        kw <- keyword
        arg <- identifier
        return (kw, arg)
  fmap KeywordPattern (P.many1 (P.try f))

-- | 3.4.2
data Temporaries = Temporaries {temporariesIdentifiers :: [LowercaseIdentifier]} deriving (Eq, Show)

-- | Number of temporaries.
temporariesLength :: Temporaries -> Int
temporariesLength (Temporaries x) = length x

-- | Empty Temporaries list.
emptyTemporaries :: Temporaries
emptyTemporaries = Temporaries []

-- | Vertical bar as lexeme (token).
verticalBar :: P Char
verticalBar = lexeme (P.char '|')

temporariesIdentifierSequence :: P [LowercaseIdentifier]
temporariesIdentifierSequence = P.between verticalBar verticalBar temporary_variable_list

{- | <temporaries> ::= '|' <temporary variable list> '|'

>>> let p = stParse temporaries
>>> p "||"
Temporaries {temporariesIdentifiers = []}

>>> p "|p|" == p "| p|"
True

>>> p "|p |" == p "| p |"
True

>>> p "|p q r|"
Temporaries {temporariesIdentifiers = ["p","q","r"]}
-}
temporaries :: P Temporaries
temporaries = P.label (fmap Temporaries temporariesIdentifierSequence) "temporaries"

{- | <temporary variable list> ::= identifier*

>>> let p = stParse temporary_variable_list
>>> p ""
[]

>>> p "p"
["p"]

>>> p "p q"
["p","q"]

>>> p "p q r" == p "p q r +"
True
-}
temporary_variable_list :: P [LowercaseIdentifier]
temporary_variable_list = P.many identifier P.<?> "temporary_variable_list"

-- * 3.4.3

-- | Comments are text strings
type Comment = String

{- | 3.4.3

The value of an initializer with no <statements> is the binding of the reserved identifier 'nil'.
-}
data InitializerDefinition
  = InitializerDefinition (Maybe Comment) (Maybe Temporaries) (Maybe Statements)
  deriving (Eq, Show)

{- | Construct a standard class initiliaze statement, i.e. "ClassName initialize"

> standardClassInitializerDefinition "ClassName"
-}
standardClassInitializerDefinition :: UppercaseIdentifier -> InitializerDefinition
standardClassInitializerDefinition nm =
  let e = ExprBasic (simpleUnaryMessageSend (PrimaryIdentifier nm) "initialize")
  in InitializerDefinition Nothing Nothing (Just (StatementsExpression e Nothing))

{- | <initializer definition> ::= [<temporaries>] [<statements>]

> p = stParse initializerDefinition
> p "|a b| a := 1 . ^ a + b ."
> p "SinOsc freq: (69 midicps) phase: 0 mul: 0.1"
> p "SinOsc freq: (69 midicps) phase: 0 mul: 0.1"
> p "|a b c| a := 1 . b := 2 . c := 3 . ^ a + b + c ."
> p "|a b c| a := [1] . b := [2] . c := [3] . ^ a value + b value + c value ."
> p "[:x | x * x] value: Float pi * 2"
> p ""
-}
initializerDefinition :: P InitializerDefinition
initializerDefinition = do
  t <- P.optionMaybe temporaries
  s <- P.optionMaybe statements
  return (InitializerDefinition Nothing t s)

nonEmptyInitializerDefinition :: P InitializerDefinition
nonEmptyInitializerDefinition = do
  InitializerDefinition c t s <- initializerDefinition
  case (t, s) of
    (Nothing, Nothing) -> P.unexpected "nonEmptyInitializerDefinition: empty"
    _ -> return (InitializerDefinition c t s)

-- | Set comment field.
initializerDefinitionSetComment :: Comment -> InitializerDefinition -> InitializerDefinition
initializerDefinitionSetComment c (InitializerDefinition _ t s) = InitializerDefinition (Just c) t s

-- * 3.4.4

{- | <block constructor> ::= '[' <block body> ']'

> p = stParse blockConstructor
> p "[1]" == p "[ 1 ]"
> p "[^1]" ==  p "[ ^ 1 .]"
> p "[:a| |t|]" == p "[ :a | | t | ]"
> p "[:a| |t u| x . y]"
> p "[:a| |t u| t := [:b| |v w| z] . x . y]"
> p "[:ignored]"
-}
blockConstructor :: P BlockBody
blockConstructor = inBrackets blockBody

{- | A block has optional arguments, optional temporaries and optional statements.
     The method name field is not assigned by the parser, see methodDefinitionAnnotateBlocks.
-}
data BlockBody = BlockBody
  { blockMethodName :: Maybe MethodName
  -- ^ Meta data
  , blockArguments :: Maybe [BlockArgument]
  , blockTemporaries :: Maybe Temporaries
  , blockStatements :: Maybe Statements
  }
  deriving (Eq, Show)

{- | Does BlockBody end with a Return (non-local).
     Note this does not look to see if the block contains any nested non-local returns.
-}
blockBodyEndsWithReturn :: BlockBody -> Bool
blockBodyEndsWithReturn = maybe False statementsEndsWithReturn . blockStatements

blockBodyArguments :: BlockBody -> [LowercaseIdentifier]
blockBodyArguments = fromMaybe [] . blockArguments

blockBodyTemporaries :: BlockBody -> [LowercaseIdentifier]
blockBodyTemporaries = maybe [] temporariesIdentifiers . blockTemporaries

blockBodyDuplicateTemporaries :: BlockBody -> [LowercaseIdentifier]
blockBodyDuplicateTemporaries m =
  let a = blockBodyArguments m
      t = blockBodyTemporaries m
  in (a ++ t) \\ nub (a ++ t)

{- | <block body> ::= [<block argument>* '|'] [<temporaries>] [<statements>]

> p = stParse blockBody
> p "" == BlockBody Nothing Nothing Nothing
> p ":a|" == p ": a|"
> p ":a :b|" == p ": a : b|"
> p ":a| |p q r|"
> p ":a :b| |p q r|"
> p "|p q r|"
> p "1"
> p ":a| a + 1"
> p ":a| |b| b := a + 1. ^b"
> p ":ignored"
> p ":a| | a | a + 1" -- duplicate name error
-}
blockBody :: P BlockBody
blockBody = do
  a <- P.optionMaybe (P.try (P.many1 blockArgument >>~ verticalBar))
  t <- P.optionMaybe temporaries
  s <- P.optionMaybe statements
  let blk = BlockBody Nothing a t s
      dup = blockBodyDuplicateTemporaries blk
  duplicateNamesError dup
  return blk

-- | An identifier for a block argument.  Written with a ':' prefix.
type BlockArgument = LowercaseIdentifier

{- | <block argument> ::= ':' identifier

> p = stParse blockArgument
> p ":a" == p ": a"
-}
blockArgument :: P BlockArgument
blockArgument = lexeme (P.char ':') >> identifier

-- * 3.4.5

-- | 3.4.5
data Statements
  = StatementsReturn ReturnStatement
  | StatementsExpression Expression (Maybe Statements)
  deriving (Eq, Show)

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

{- | '.' as lexeme (token)

> stParse period "." == "."
-}
period :: P String
period = Token.dot stLexer

{- | <statements> ::= (<return statement> ['.']) | (<expression> ['.' [<statements>]])

> p = stParse statements
> p "^p" == p "^ p"
> p "^p." == p "^p ."
> p "^ p." == p "^ p ."
> p "p"
> p "p q"
> p "(p q)"
> p "p.^q" == p "p .^q"
> p "p . ^q" == p "p . ^ q"
> p "^ 440 * (2 ** ((self - 69) * (1 / 12)))"
> p "p. q." == p "p. q"
-}
statements :: P Statements
statements = do
  let rhs = do
        e <- expression
        p <- P.optionMaybe period
        s <- case p of
          Nothing -> return Nothing
          Just _ -> P.optionMaybe statements
        return (StatementsExpression e s)
  P.choice [fmap StatementsReturn (returnStatement >>~ P.optional period), rhs]

-- | 3.4.5.1 Return statement
data ReturnStatement = ReturnStatement Expression deriving (Eq, Show)

{- | <return statement> ::= returnOperator <expression>

> p = stParse returnStatement
> p "^1"
> p "^p"
> p "^ 1"
> p "^ p"
> p "^ a value + b value + c value"
> p "^ self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
-}
returnStatement :: P ReturnStatement
returnStatement = fmap ReturnStatement (returnOperator >> expression)

-- | 3.4.5.2 Expressions
data Expression = ExprAssignment Assignment | ExprBasic BasicExpression deriving (Eq, Show)

expressionCase :: (Assignment -> t) -> (BasicExpression -> t) -> Expression -> t
expressionCase f g e =
  case e of
    ExprAssignment x -> f x
    ExprBasic x -> g x

{- | <expression> ::= <assignment> | <basic expression>

> p = stParse expression
> p "p := 1"
> p "p := q"
> p "p"
> p "1"
> p "1 + 2"
> p "p := (1 + 2)"
> p "p := (1 + 2) negate"
> p "a value + b value + c value"
> p "note2 := (sequR value: #(-12 -7 -5 0 2 5) value: clock_16) + note1"
> p "freq := ((#(-2 0 3 5 7 10 12 15) at: i) + 60) midicps ."
> p "(1 to: 6) do: [:i| s := AllpassN in: s maxdelaytime: 0.1 delaytime: {0.05 rand . 0.05 rand} decaytime: 4] . \"rvb\""
> p "out := pitch size mixFill: [:i| |trigger pluck period string| trigger := HPZ1 in: (mousex > (0.25 + ((i - 1) * triggerSpacing))) abs . x]"
> p "out := pitch size mixFill: [:i| x]"
> p "pitch size mixFill: x"
> p "[1] value"
> p "o m: true"
> p "self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
-}
expression :: P Expression
expression = fmap ExprAssignment (P.try assignment) P.<|> fmap ExprBasic basicExpression

{- | 3.4.5.2 (Expressions)
Class variables may be assigned to, and they are upper case.
-}
data Assignment = Assignment Identifier Expression deriving (Eq, Show)

{- | <assignment> ::= <assignment target> assignmentOperator <expression>

> p = stParse assignment
> p "p:=1" == p "p :=1"
> p "p:= 1" == p "p := 1"
> p "p := q"
> p "p := 2.0"
> p "p := 'x'"
> p "p := 8 mixFill: [:i| |n trigger pluck freq z metal| n := 15 ]"
> p "p := q := r := nil"
-}
assignment :: P Assignment
assignment = do
  a <- assignmentTarget
  _ <- assignmentOperator
  e <- expression
  return (Assignment a e)

-- | 3.4.5.2
data BasicExpression
  = BasicExpression Primary (Maybe Messages) (Maybe CascadedMessages)
  deriving (Eq, Show)

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

{- | <basic expression> ::= <primary> [<messages> <cascaded messages>]

> let p = stParse basicExpression
> p "1"
> p "p"
> p "1 negate"
> p "p negate"
> p "1 + 2"
> p "(p)"
> p "(p q)"
> p "a value + b value + c value"
> p "p q r: x" == p "p q r:x"
> p "self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
> p "self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
> p "w * ((x + y) z)"
> p "w * (x + y) z"
> p "Transcript\n \"...\" show: 'hello ';\n show: 'world';\n cr."
> p "Point new x: 25 y: 35; isZero"
> p "self ifNil:" -- error
-}
basicExpression :: P BasicExpression
basicExpression = do
  p <- primary
  m <- P.optionMaybe messages
  c <- cascadedMessages
  return (BasicExpression p m (if null c then Nothing else Just c))

{- | <assignment target> := identifier

> p = stParse assignmentTarget
> p "self" -- ? ; error ; reserved words cannot be assigned to
-}
assignmentTarget :: P Identifier
assignmentTarget = P.label identifier "assignmentTarget"

-- | 3.4.5.2 (Expressions)
data Primary
  = PrimaryIdentifier Identifier
  | PrimaryLiteral Literal
  | PrimaryBlock BlockBody
  | PrimaryExpression Expression
  | PrimaryArrayExpression [BasicExpression] -- non-Ansi
  deriving (Eq, Show)

-- | 3.4.5.3 Messages
data Messages
  = MessagesUnary [UnaryMessage] (Maybe [BinaryMessage]) (Maybe KeywordMessage)
  | MessagesBinary [BinaryMessage] (Maybe KeywordMessage)
  | MessagesKeyword KeywordMessage
  deriving (Eq, Show)

inParentheses :: P t -> P t
inParentheses = Token.parens stLexer

inBraces :: P t -> P t
inBraces = Token.braces stLexer

inBrackets :: P t -> P t
inBrackets = Token.brackets stLexer

{- | <primary> ::= identifier | <literal> | <block constructor> | ( '(' <expression> ')' )

> let p = stParse primary
> p "p" == PrimaryIdentifier "p"
> p "1"
> p "[:a| a + 1]"
> p "(p)"
> p "(p q)"
> p "(p q: r)"
> p "(1 + 2)"
> map p ["{p.q.r}", "{p. q. r}", "{ p . q . r }", "{ p . q . r . }", "{p.q.r.}"]
> map p ["{p. {q. r} }", "{p. {q. r.} .}"]
-}
primary :: P Primary
primary =
  lexeme
    ( P.choice
        [ fmap PrimaryBlock blockConstructor
        , fmap PrimaryExpression (inParentheses expression)
        , fmap PrimaryArrayExpression (inBraces (P.sepEndBy basicExpression period)) -- allow ending period?
        , fmap PrimaryIdentifier identifier
        , fmap PrimaryLiteral literal
        ]
        P.<?> "primary"
    )

{- | <unary message>+ <binary message>* [<keyword message>]

> p = stParse unaryMessages
> p "abs"
> p "abs square"
> p "abs square +p" == p "abs square + p"
> p "abs square +p +q" == p "abs square + p + q"
> p "value +b value +c value" == p "value + b value + c value"
> p "abs square +p +q k:r" == p "abs square + p + q k: r"
> p "abs square +p +q k1:p1 k2:p2" == p "abs square + p + q k1: p1 k2: p2"
> p "q r:x" == p "q r: x"
-}
unaryMessages :: P Messages
unaryMessages = do
  u <- P.many1 (P.try unaryMessage)
  b <- P.many binaryMessage
  k <- P.optionMaybe keywordMessage
  return (MessagesUnary u (if null b then Nothing else Just b) k)

-- | <binary message>+ [<keyword message>]
binaryMessages :: P Messages
binaryMessages = do
  b <- P.many1 binaryMessage
  k <- P.optionMaybe keywordMessage
  return (MessagesBinary b k)

{- | <messages> ::= (<unary message>+ <binary message>* [<keyword message>] )
                  | (<binary message>+ [<keyword message>] )
                  | <keyword message>

> rw = messages_pp . stParse messages
> (rw "k1:p1",rw "k1:p1 k2:p2")
> (rw "+p",rw "+p +q")
> (rw "+p k:r",rw "+p +q k:r")

> p = stParse messages
> p "k1:p1" == p "k1: p1"
> p "k1: p1 k2: p2" == p "k1:p1 k2:p2"
> p "+1"== p "+ 1"
> p "+p" == p "+ p"
> p "+p +q" == p "+ p + q"
> p "+p +q k:r" == p "+ p + q k: r"
> p "q r: x" == p "q r:x"
> p "< 0 ifTrue: [0 - self] ifFalse: [self]" == p "<0ifTrue:[0-self]ifFalse:[self]"
> p "x perform: #y: with: z" -- error
-}
messages :: P Messages
messages = P.choice [P.try binaryMessages, P.try (fmap MessagesKeyword keywordMessage), unaryMessages]

data UnaryMessage = UnaryMessage LowercaseIdentifier deriving (Eq, Show)

unaryMessageSelector :: UnaryMessage -> Selector
unaryMessageSelector (UnaryMessage u) = UnarySelector u

data BinaryMessage
  = BinaryMessage BinaryIdentifier BinaryArgument
  deriving (Eq, Show)

binaryMessageSelector :: BinaryMessage -> Selector
binaryMessageSelector (BinaryMessage b _) = BinarySelector b

data KeywordMessage
  = KeywordMessage [(Keyword, KeywordArgument)]
  deriving (Eq, Show)

-- | Keyword selector from KeywordMessage.
keywordMessageSelector :: KeywordMessage -> Selector
keywordMessageSelector (KeywordMessage l) = KeywordSelector (concatMap fst l) (length l)

{- | <unary message> ::= unarySelector

> stParse unaryMessage "p" == UnaryMessage "p"
> stParse unaryMessage "" -- FAIL
-}
unaryMessage :: P UnaryMessage
unaryMessage = fmap (UnaryMessage . selectorIdentifier) unarySelector -- lexeme

{- | <binary message> ::= binarySelector <binary argument>

> stParse binaryMessage "+p" == stParse binaryMessage "+ p"
> stParse binaryMessage "+1" == stParse binaryMessage "+ 1"
> stParse binaryMessage "+1.0" == stParse binaryMessage "+ 1.0"
> stParse binaryMessage "+ 1 k:" -- + 1
-}
binaryMessage :: P BinaryMessage
binaryMessage = do
  BinarySelector sel <- binarySelector
  arg <- binaryArgument
  return (BinaryMessage sel arg)

data BinaryArgument
  = BinaryArgument Primary (Maybe [UnaryMessage])
  deriving (Eq, Show)

{- | <binary argument> ::= <primary> <unary message>*

> stParse binaryArgument "p"
> stParse binaryArgument "p q"
> stParse binaryArgument "p q r"
> stParse binaryArgument "p q r s:" -- p q r
-}
binaryArgument :: P BinaryArgument
binaryArgument = do
  p <- primary
  u <- P.optionMaybe (P.many (P.try unaryMessage))
  return (BinaryArgument p u)

{- | <keyword message> ::= (keyword <keyword argument>)+

> p = stParse keywordMessage
> p "k:p" == p "k: p"
> p "k1:p1 k2:p2" == p "k1: p1 k2: p2"
> p "k1: #p1:"
> p "k1: #p1: k2: #p2"
-}
keywordMessage :: P KeywordMessage
keywordMessage = do
  let f = do
        kw <- keyword
        arg <- keywordArgument
        return (kw, arg)
  fmap KeywordMessage (P.many1 f)

data KeywordArgument
  = KeywordArgument Primary (Maybe [UnaryMessage]) (Maybe [BinaryMessage])
  deriving (Eq, Show)

{- | <keyword argument> ::= <primary> <unary message>* <binary message>*

> p = stParse keywordArgument
> p "p"
> p "1"
> p "p q"
> p "p q r"
> p "p q r + s"
> p "p q r + s * t"
> p "#m: w:" == p "#m:"
> p "p k:" == p "p"
-}
keywordArgument :: P KeywordArgument
keywordArgument = do
  p <- primary
  u <- P.optionMaybe (P.many1 (P.try unaryMessage))
  b <- P.optionMaybe (P.many1 (P.try binaryMessage))
  return (KeywordArgument p u b)

cascadeSeparator :: P String
cascadeSeparator = Token.semi stLexer

type CascadedMessages = [Messages]

-- | <cascaded messages> ::= (';' <messages>)*
cascadedMessages :: P CascadedMessages
cascadedMessages = P.many (cascadeSeparator >> messages)

-- * 3.4.6

type Symbol = String

{- | <literal> ::= <number literal>
                 | <string literal>
                 | <character literal>
                 | <symbol literal>
                 | <selector literal>
                 | <array literal>
-}
data Literal
  = NumberLiteral Number
  | StringLiteral String
  | CharacterLiteral Char
  | SymbolLiteral Symbol
  | SelectorLiteral Selector
  | ArrayLiteral [Either Literal Identifier]
  deriving (Eq, Show)

literalInteger :: Literal -> Maybe Integer
literalInteger lit =
  case lit of
    NumberLiteral (Int k) -> Just k
    _ -> Nothing

integerLiteral :: Integer -> Literal
integerLiteral = NumberLiteral . Int

{- | Parse literal.

>>> p = stParse literal
>>> p "123"
NumberLiteral (Int 123)

>>> p "-123"
NumberLiteral (Int (-123))

>>> p "123.456"
NumberLiteral (Float 123.456)

>>> p "-123.456"
NumberLiteral (Float (-123.456))

>>> p "'x'"
StringLiteral "x"

>>> p "$x"
CharacterLiteral 'x'

>>> p "#'xyz'"
SymbolLiteral "xyz"

>>> p "#abs"
SelectorLiteral (UnarySelector "abs")

>>> p "#m:"
SelectorLiteral (KeywordSelector "m:" 1)

>>> p "#freq:iphase:"
SelectorLiteral (KeywordSelector "freq:iphase:" 2)

>>> p "#+"
SelectorLiteral (BinarySelector "+")

> p "#(1 2.0 'x') $x #'xyz' #abs #freq:iphase: #+)"

> p "#(-12 -7 -5 0 2 5)"

> p "#(x y: #z: 1)"

> p "x"
-}
literal :: P Literal
literal = P.choice [numberLiteral, stringLiteral, characterLiteral, P.try arrayLiteral, P.try symbolLiteral, selectorLiteral] -- lexeme

{- | <number literal> ::= ['-'] <number>

>>> stParse numberLiteral "123"
NumberLiteral (Int 123)

>>> stParse numberLiteral "123.456"
NumberLiteral (Float 123.456)

>>> stParse numberLiteral "-123"
NumberLiteral (Int (-123))

>>> stParse numberLiteral "-123.456"
NumberLiteral (Float (-123.456))

>>> stParse numberLiteral "1e-2"
NumberLiteral (Float 1.0e-2)
-}
numberLiteral :: P Literal
numberLiteral = do
  n <- P.optionMaybe (P.char '-')
  let rw :: Num n => n -> n
      rw = maybe id (const negate) n
  fmap (NumberLiteral . numberCase (Int . rw) (Float . rw)) number -- lexeme

data Number = Int Integer | Float Double deriving (Eq, Show)

numberCase :: (Integer -> t) -> (Double -> t) -> Number -> t
numberCase f1 f2 n =
  case n of
    Int x -> f1 x
    Float x -> f2 x

numberFloat :: Number -> Double
numberFloat = numberCase fromIntegral id

{- | <number> ::= integer | float | scaledDecimal

>>> map (stParse number) (words "1 1.2")
[Int 1,Float 1.2]

> map (stParse number) (words "-1 -1.2") -- FAIL
-}
number :: P Number
number = fmap Float (P.try float) P.<|> fmap Int integer -- lexeme

-- | <character literal> ::= quotedCharacter
characterLiteral :: P Literal
characterLiteral = fmap CharacterLiteral quotedCharacter

{- | <string literal> ::= quotedString

>>> let p = stParse stringLiteral
>>> p "'x'"
StringLiteral "x"

>>> p "'\\n'"
StringLiteral "\\n"
-}
stringLiteral :: P Literal
stringLiteral = fmap StringLiteral quotedString

-- | <symbol literal> ::= hashedString
symbolLiteral :: P Literal
symbolLiteral = fmap SymbolLiteral hashedString

-- | <selector literal> ::= quotedSelector
selectorLiteral :: P Literal
selectorLiteral = fmap SelectorLiteral quotedSelector

-- | #( is the start token for literal arrays.
hashOpenParen :: P String
hashOpenParen = lexeme (P.string "#(")

openParen :: P Char
openParen = lexeme (P.char '(')

closeParen :: P Char
closeParen = lexeme (P.char ')')

{- | 3.4.6.6
     <array literal> ::= '#(' <array element>* ')'

>>> let p = stParse arrayLiteral
>>> p "#()" == p "#( )"
True

>>> p "#(1)" == p "#( 1 )"
True

>>> p "#(1 2.0)"
ArrayLiteral [Left (NumberLiteral (Int 1)),Left (NumberLiteral (Float 2.0))]

>>> p "#(1 2.0 3)"
ArrayLiteral [Left (NumberLiteral (Int 1)),Left (NumberLiteral (Float 2.0)),Left (NumberLiteral (Int 3))]

>>> p "#(1 2.0 true)"
ArrayLiteral [Left (NumberLiteral (Int 1)),Left (NumberLiteral (Float 2.0)),Right "true"]

>>> p "#(1 #(2 3) 4)" == p "#(1 (2 3) 4)"
True

>>> p "#(x x: #x: nil)" == p "#(#'x' #'x:' #x: nil)"
True
-}
arrayLiteral :: P Literal
arrayLiteral = fmap ArrayLiteral (P.between hashOpenParen closeParen (P.many arrayElement))

{- | Allow array elements within literal arrays to elide the # at the opening parenthesis.
      This is the rule in the ST-80 sources.
      Non-Ansi.
-}
interiorArrayLiteral :: P Literal
interiorArrayLiteral = fmap ArrayLiteral (P.between openParen closeParen (P.many arrayElement))

{- | Allow identifiers (both ordinary and keyword) within literal arrays to elide the #.
      This is the rule in the ST-80 sources.
      Non-Ansi.

> map (stParse interiorSymbol) (words "x y: #z:")
-}
interiorSymbol :: P Literal
interiorSymbol = fmap SymbolLiteral (P.try keyword P.<|> identifier)

{- | <array element> ::= <literal> | identifier

Ansi:
If an identifier appears as an <array element> and it is one of the
reserved identifiers nil, true or false the value of the corresponding
element of the collection is the value of that reserved
identifier. The meaning is undefined if any other identifier is used
as an <array element>

ST-80:
The Ansi rule is not the rule from ST-80, where #(x x: nil) means #(#'x' #'y:' nil).

>>> let p = stParse arrayElement
>>> p "1"
Left (NumberLiteral (Int 1))

>>> p "2.0"
Left (NumberLiteral (Float 2.0))

>>> p "nil"
Right "nil"

>>> p "(1 2.0 nil symbol keyword: #hashedKeyword: -1)"
Left (ArrayLiteral [Left (NumberLiteral (Int 1)),Left (NumberLiteral (Float 2.0)),Right "nil",Left (SymbolLiteral "symbol"),Left (SymbolLiteral "keyword:"),Left (SelectorLiteral (KeywordSelector "hashedKeyword:" 1)),Left (NumberLiteral (Int (-1)))])

>>> p "x"
Left (SymbolLiteral "x")

>>> p "x:"
Left (SymbolLiteral "x:")

>>> p "#x:"
Left (SelectorLiteral (KeywordSelector "x:" 1))

>>> stParse (P.many arrayElement) "1 '2' 3.14 x #'y' -1 #z: -2"
[Left (NumberLiteral (Int 1)),Left (StringLiteral "2"),Left (NumberLiteral (Float 3.14)),Left (SymbolLiteral "x"),Left (SymbolLiteral "y"),Left (NumberLiteral (Int (-1))),Left (SelectorLiteral (KeywordSelector "z:" 1)),Left (NumberLiteral (Int (-2)))]
-}
arrayElement :: P (Either Literal Identifier)
arrayElement = fmap Right reservedIdentifier P.<|> fmap Left (literal P.<|> interiorArrayLiteral P.<|> interiorSymbol) -- lexeme

-- * 3.4.7 Reserved Identifiers

{- | These are the reserved identifiers.
     self and super are not of the same kind with regards to unquoted symbols in literal arrays.
-}
stReservedIdentifiers :: [String]
stReservedIdentifiers = words "nil true false self super"

-- * 3.5.1

-- | character ::= "Any character in the implementation-defined character set"
character :: P Char
character = P.anyChar

-- | whitespace ::= "Any non-printing character interpreted as white space including spaces, tabs, and line breaks"
whitespace :: P Char
whitespace = P.space

-- | digit ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
digit :: P Char
digit = P.digit

-- | uppercaseAlphabetic ::= 'A' .. 'Z'
uppercaseAlphabetic :: P Char
uppercaseAlphabetic = P.upper

-- | lowercaseAlphabetic ::= 'a' .. 'z'
lowercaseAlphabetic :: P Char
lowercaseAlphabetic = P.lower

-- | nonCaseLetter ::= '_'
nonCaseLetter :: P Char
nonCaseLetter = P.char '_'

{- | letter ::= uppercaseAlphabetic
              | lowercaseAlphabetic
              | nonCaseLetter
              | "implementation defined letters"
-}
letter :: P Char
letter = P.choice [uppercaseAlphabetic, lowercaseAlphabetic, nonCaseLetter]

-- * 3.5.2

-- | commentDelimiter ::= '"'
commentDelimiter :: P Char
commentDelimiter = P.char '"' -- non-lexeme (the lhs comment marker is not a lexeme, the rhs is)

{- | nonCommentDelimiter::= "any character that is not a commentDelimiter "

>>> stParse nonCommentDelimiter "x"
'x'

> stParse nonCommentDelimiter "\"" -- FAIL
-}
nonCommentDelimiter :: P Char
nonCommentDelimiter = P.noneOf ['"']

{- | comment := commentDelimiter nonCommentDelimiter * commentDelimiter

>>> stParse comment "\"\""
""

>>> stParse comment "\" x\""
" x"

>>> stParse comment "\"x \""
"x "

>>> stParse comment "\"analog bubbles (jmcc) #1\""
"analog bubbles (jmcc) #1"
-}
comment :: P String
comment = P.between commentDelimiter (lexeme commentDelimiter) (P.many nonCommentDelimiter)

-- * 3.5.3

-- | C.f. 3.4.7
type ReservedIdentifier = Identifier

{- | One of stReservedIdentifiers

>>> let p = stParse reservedIdentifier
>>> map p (words "self super nil")
["self","super","nil"]

> p "x" -- error
-}
reservedIdentifier :: P ReservedIdentifier
reservedIdentifier = lexeme (P.choice (map (P.try . P.string) stReservedIdentifiers))

type OrdinaryIdentifier = String

{- | An identifier that is not a reserved identifier

>>> let p = stParse ordinaryIdentifier
>>> p "x"
"x"

>>> p "x_"
"x_"

>>> p "x0"
"x0"

> p "self" -- error
-}
ordinaryIdentifier :: P OrdinaryIdentifier
ordinaryIdentifier = Token.identifier stLexer

--  if token `elem` stReservedIdentifiers then P.unexpected ("reservedIdentifier: " ++ token) else return token

underscore :: P Char
underscore = P.char '_'

{- | A non-lexeme variant of ordinaryIdentifier.  For keyword.

>>> let p = stParse identifierNonLexemeForKeyword
>>> p "x"
"x"

>>> p "x_1"
"x_1"

>>> p "super" -- super: as part of a keyword is allowed, &etc.
"super"
-}
identifierNonLexemeForKeyword :: P OrdinaryIdentifier
identifierNonLexemeForKeyword = do
  c0 <- P.letter
  cN <- P.many (P.letter P.<|> underscore P.<|> P.digit)
  return (c0 : cN)

type Identifier = String

{- | identifier ::= letter (letter | digit)*

Ansi doesn't allow underscore but Squeak does, also some Sc Ugens have underscores in bot the class (PV_) and parameter names.

>>> let p = stParse identifier
>>> p "x1"
"x1"

>>> p "X1"
"X1"

>>> p "x_1"
"x_1"

> p "1x" -- FAIL

> p "" -- FAIL

>>> p "true"
"true"

>>> p "nil"
"nil"

>>> p "y:"
"y"

> p "#y:" -- error
-}
identifier :: P Identifier
identifier = ordinaryIdentifier P.<|> reservedIdentifier

-- * 3.5.4

{- | An identifier ending with a colon.

Keywords are identifiers followed immediately by the colon
character. An unadorned identifier is an identifier which is not
immediately preceded by a '#'. If a ':' followed by an '=' immediately
follows an unadorned identifier, with no intervening white space, then
the token is to be parsed as an identifier followed by an
assignmentOperator not as an keyword followed by an '='.
-}
type Keyword = LowercaseIdentifier

-- | A keyword parser that is not a lexeme.  A keyword selector is a sequence of non-lexeme keywords.
keywordNotLexeme :: P Keyword
keywordNotLexeme = do
  cs <- P.label identifierNonLexemeForKeyword "keyword"
  c <- P.label (P.char ':') "keyword"
  return (cs ++ [c])

{- | keyword ::= identifier ':'

>>> let p = stParse keyword
>>> p "kw:"
"kw:"

> p "self:" -- error ?
> p "kw :" -- error
> p "kw" -- error
-}
keyword :: P Keyword
keyword = lexeme keywordNotLexeme

-- * 3.5.5

-- | The list of characters that form operator names.
binaryCharacterSet :: [Char]
binaryCharacterSet = "!%&*+,/<=>?@\\~|-"

{- | binaryCharacter ::= '!' | '%' | '&' | '*' | '+' | ',' | '/'
                       | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'
-}
binaryCharacter :: P Char
binaryCharacter = P.oneOf binaryCharacterSet

-- | Identifier for binary operations.
type BinaryIdentifier = Identifier

{- | Is an identifier a binary operator?

>>> map isBinaryIdentifier (words "+ - * / % @ ** | &")
[True,True,True,True,True,True,True,True,True]
-}
isBinaryIdentifier :: Identifier -> Bool
isBinaryIdentifier = all (`elem` binaryCharacterSet)

{- | binarySelector ::= binaryCharacter+

>>> stParse binarySelector "&"
BinarySelector "&"

>>> stParse binarySelector "+p" -- +1 must parse as selector=+ argument=1
BinarySelector "+"
-}
binarySelector :: P Selector
binarySelector = fmap BinarySelector (Token.operator stLexer)

-- | returnOperator ::= '^'
returnOperator :: P Char
returnOperator = lexeme (P.char '^')

-- | assignmentOperator ::= ':='
assignmentOperator :: P String
assignmentOperator = lexeme (P.string ":=")

-- * 3.5.6

{- | integer ::= decimalInteger | radixInteger

>>> let p = stParse integer
>>> map p (words "63 8r77 99 10r99 255 16rFF")
[63,63,99,99,255,255]
-}
integer :: P Integer
integer = P.try radixInteger P.<|> decimalInteger

{- | radixInteger ::= radixSpecifier 'r' radixDigits

>>> let p = stParse radixInteger
>>> map p (words "8r77 10r99 16rFF")
[63,99,255]

>>> map p (words "2r10111 8r27 10r23 16r17")
[23,23,23,23]
-}
radixInteger :: P Integer
radixInteger = fmap (fromMaybe (error "radixInteger?")) radixIntegerMaybe

radixIntegerMaybe :: P (Maybe Integer)
radixIntegerMaybe = do
  rs <- radixSpecifier
  _ <- P.char 'r'
  rd <- lexeme radixDigits
  let get x =
        case x of
          [(answer, "")] -> Just answer
          _ -> Nothing
      n =
        case rs of
          2 -> get (readBin rd) -- base=4.16.1
          8 -> get (readOct rd)
          10 -> get (readDec rd)
          16 -> get (readHex rd)
          _ -> Nothing
  return n

-- | radixSpecifier := digits
radixSpecifier :: P Integer
radixSpecifier = fmap read digits

-- | radixDigits ::= (digit | uppercaseAlphabetic)+
radixDigits :: P String
radixDigits = P.many1 (digit P.<|> uppercaseAlphabetic)

-- | decimalInteger ::= digits
decimalInteger :: P Integer
decimalInteger = fmap read digits

-- | digits ::= digit+
digits :: P String
digits = lexeme (P.many1 P.digit)

-- | float ::= mantissa [exponentLetter exponent]
float :: P Double
float = Token.float stLexer

-- * 3.5.7

type QuotedCharacter = Char

{- | quotedCharacter ::= '$' character

>>> stParse quotedCharacter "$x"
'x'
-}
quotedCharacter :: P Char
quotedCharacter = P.label (P.char '$' >> lexeme P.anyChar) "quotedCharacter"

-- * 3.5.8

-- | String quote character is '
type QuotedString = String

{- | Remove quote characters from QuotedString.

>>> unquoteQuotedString "'string'"
"string"

>>> unquoteQuotedString "'instance creation'"
"instance creation"
-}
unquoteQuotedString :: QuotedString -> String
unquoteQuotedString x = take (length x - 2) (drop 1 x)

{- | quotedString ::= stringDelimiter stringBody stringDelimiter

>>> let p = stParse quotedString
>>> p "''"
""

>>> p "''''"
"'"

>>> p "' xy'"
" xy"

>>> p "'x''y'"
"x'y"

>>> p "'''x''y'''"
"'x'y'"

>>> p "'\n'"
"\n"

>>> p "'x\n'"
"x\n"

>>> p "'\0x\n'"
"\NULx\n"

>>> p "'\\n'"
"\\n"
-}
quotedString :: P QuotedString
quotedString = P.between stringDelimiter (lexeme stringDelimiter) stringBody -- lexeme

{- | stringBody ::= (nonStringDelimiter | (stringDelimiter stringDelimiter)*)

>>> let p = stParse stringBody
>>> p ""
""

>>> p "x y"
"x y"

>>> p "x"
"x"

>>> p "x''"
"x'"

>>> p "x'"
"x"
-}
stringBody :: P String
stringBody = P.many (P.try escapedStringDelimiter P.<|> nonStringDelimiter)

-- | stringDelimiter ::= ''' "a single quote"
stringDelimiter :: P Char
stringDelimiter = P.char '\'' -- non-lexeme (the lhs is not a lexeme, the rhs is)

-- | nonStringDelimiter ::= "any character except stringDelimiter"
nonStringDelimiter :: P Char
nonStringDelimiter = P.noneOf "'"

-- | Ansi escaped quote (quote quote).
escapedStringDelimiter :: P Char
escapedStringDelimiter = stringDelimiter >> stringDelimiter

-- * 3.5.9

type HashedString = String

{- | hashedString ::= '#' quotedString

>>> stParse hashedString "#'x'"
"x"
-}
hashedString :: P HashedString
hashedString = (P.char '#' >> quotedString) P.<?> "hashedString"

-- * 3.5.10

{- | Identifier (lexeme), not ending with ':'

>>> stParse unarySelector "p"
UnarySelector "p"

> stParse unarySelector "p:" -- FAIL
-}
unarySelector :: P Selector
unarySelector =
  (identifier >>= \u -> P.notFollowedBy (P.char ':') >> return (UnarySelector u))
    P.<?> "unarySelector"

data Selector
  = UnarySelector LowercaseIdentifier
  | BinarySelector BinaryIdentifier
  | KeywordSelector LowercaseIdentifier Int
  deriving (Eq, Ord, Show)

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

{- | quotedSelector ::= '#' (unarySelector | binarySelector | keywordSelector)

>>> let p = stParse quotedSelector
>>> p "#abs"
UnarySelector "abs"

>>> p "#freq:"
KeywordSelector "freq:" 1

>>> p "#freq:iphase:"
KeywordSelector "freq:iphase:" 2

>>> p "#+"
BinarySelector "+"

>>> p "#+:" == p "#+" -- ?
True

>>> p "#+x" == p "#+" -- ?
True

> p "#'class'" -- error
-}
quotedSelector :: P Selector
quotedSelector =
  let sel = P.choice [binarySelector, P.try keywordSelector, unarySelector]
  in P.label (P.char '#' >> sel) "quotedSelector" -- lexeme

{- | keywordSelector ::= keyword+

>>> let p = stParse keywordSelector
>>> p "freq:"
KeywordSelector "freq:" 1

>>> p "freq:iphase:"
KeywordSelector "freq:iphase:" 2

>>> p "p: q:"
KeywordSelector "p:" 1

> p "freq:iphase:x" -- error
-}
keywordSelectorNotLexeme :: P Selector
keywordSelectorNotLexeme =
  fmap (asKeywordSelector . concat) (P.many1 keywordNotLexeme) P.<?> "keywordSelector"

keywordSelector :: P Selector
keywordSelector = lexeme keywordSelectorNotLexeme

-- * 3.5.11

{- | separator ::= (whitespace | comment)*

>>> stParse separator " "
" "

>>> stParse separator "\t"
"\t"

>>> stParse separator "\n"
"\n"

>>> stParse separator "  "
"  "

>>> stParse separator " \t"
" \t"

>>> stParse separator "\"commentary\""
"commentary"

>>> stParse separator "\t\"commentary\""
"\tcommentary"

> stParse separator "" -- FAIL
-}
separator :: P String
separator = fmap concat (P.many1 (fmap return P.space P.<|> comment) P.<?> "separator")

-- * Primitive

{- | Parse Squeak/Gnu type Vm primitive. (Non-Ansi).
Allow unquoted symbol, which will be printed back unquoted (as would a quoted symbol).

>>> stParse primitive "<primitive: 63>"
Primitive {primitiveLabel = NumberLiteral (Int 63)}

>>> stParse primitive "<primitive: VMpr_ByteString_at>"
Primitive {primitiveLabel = SymbolLiteral "VMpr_ByteString_at"}
-}
data Primitive = Primitive {primitiveLabel :: Literal} deriving (Eq, Show)

-- | Primitive with integer label.
primitiveOf :: Integer -> Primitive
primitiveOf = Primitive . integerLiteral

primitive :: P Primitive
primitive = do
  _ <- lexeme (P.char '<')
  _ <- lexeme (P.string "primitive")
  _ <- lexeme (P.char ':')
  l <- literal P.<|> interiorSymbol
  _ <- lexeme (P.char '>')
  return (Primitive l)
