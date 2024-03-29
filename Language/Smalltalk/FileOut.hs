{- | FileOut Code Format

<http://wiki.squeak.org/squeak/1105>

A fileout consists of a sequence of code segments called "chunks" separated by a ! character.

Any ! character occurring within the code segment must be quoted as !!.
This includes the $! character literal, and ! in quoted strings and comments.

An empty chunk consisting of one or more whitespace characters terminates the sequence.

These sequence takes one of two forms:

  <chunk>!<chunk>! ... !<whitespace>!
  !<reader>!<stream>!

The parser here accepts only a subset of the second form,
that required to add methods (methodsFor:) or comments (commentStamp:) to classes.
It assumes that stream is a sequence of chunks ending with an empty chunk.
-}
module Language.Smalltalk.FileOut where

import Data.Either {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Text.Parsec as P {- parsec -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

-- * Types

-- | A chunk is a text string.
type Chunk = String

-- | A reader is a text string.
type Reader = String

-- | Whitespace is a text string containing only space characters.
type Whitespace = String

-- | A FileOut segment is either an Eval segment or a Reader segment.
data FileOutSegment
  = FileOutEvalSegment Chunk
  | FileOutReaderSegment Reader [Chunk]
  deriving (Eq, Show)

-- | A FileOut is a sequence of segments.
type FileOut = [FileOutSegment]

-- | FileOut entries recognised.
data FileOutEntry
  = -- | superclass, class, subclass kind, [instance variable], [class variable], [pool dictionary], class category
    FileOutClassDeclaration St.Identifier St.Identifier St.Indexable [St.Identifier] [St.Identifier] [St.Identifier] String
  | -- | class, comment
    FileOutClassComment St.Identifier String
  | -- | class
    FileOutClassInitializer St.Identifier
  | -- | class, classmethods?, category, [methods]
    FileOutMethodsFor St.Identifier Bool String [St.MethodDefinition]
  | -- | class, variables
    FileOutClassClassInstanceVariables St.Identifier [St.Identifier]
  deriving (Eq, Show)

-- | A FileOut library is the list of recognised entries.
type FileOutLibrary = [FileOutEntry]

isFileOutClassDeclaration :: FileOutEntry -> Bool
isFileOutClassDeclaration e =
  case e of
    FileOutClassDeclaration {} -> True
    _ -> False

isFileOutMethodsForClass :: St.Identifier -> FileOutEntry -> Bool
isFileOutMethodsForClass className e =
  case e of
    FileOutMethodsFor className' _ _ _ -> className == className'
    _ -> False

{- | All entries are related to a class.  Retrieves class name.
     For class declarations this is the class being declared, not the super class.
-}
fileOutEntryClass :: FileOutEntry -> St.Identifier
fileOutEntryClass e =
  case e of
    FileOutClassDeclaration _ x _ _ _ _ _ -> x
    FileOutClassComment x _ -> x
    FileOutClassInitializer x -> x
    FileOutMethodsFor x _ _ _ -> x
    FileOutClassClassInstanceVariables x _ -> x

{- | The set of all classes that have entries.
     This includes classes that have method definitions but no declarations.
-}
fileOutLibraryClassSet :: FileOutLibrary -> [St.Identifier]
fileOutLibraryClassSet = nub . sort . map fileOutEntryClass

-- | The names of the set of classes that are defined (declared).
fileOutLibraryClassesDefined :: FileOutLibrary -> [St.Identifier]
fileOutLibraryClassesDefined = nub . sort . map fileOutEntryClass . filter isFileOutClassDeclaration

-- | The class definitions of the set of classes that are defined (declared).
fileOutLibraryClassDefinitions :: FileOutLibrary -> [St.ClassDefinition]
fileOutLibraryClassDefinitions e = map (fileOutLibraryClassDefinitionOrError e) (fileOutLibraryClassesDefined e)

-- | The set of classes that are extended (have method definitions but are not declared).
fileOutLibraryClassesExtended :: FileOutLibrary -> [St.Identifier]
fileOutLibraryClassesExtended l =
  let def = fileOutLibraryClassesDefined l
      ent = fileOutLibraryClassSet l
  in sort (ent \\ def)

-- | Type string of entry, for pretty printing.
fileOutEntryType :: FileOutEntry -> String
fileOutEntryType e =
  case e of
    FileOutClassDeclaration {} -> "ClassDeclaration"
    FileOutClassComment {} -> "ClassComment"
    FileOutClassInitializer {} -> "ClassInitializer"
    FileOutMethodsFor _ isClass _ _ -> (if isClass then "Class" else "Instance") ++ "MethodsFor"
    FileOutClassClassInstanceVariables {} -> "ClassClassInstanceVariables"

-- | Method definitions of entry, or error if not a MethodsFor entry.
fileOutEntryMethodDefinitions :: FileOutEntry -> [St.MethodDefinition]
fileOutEntryMethodDefinitions e =
  case e of
    FileOutMethodsFor _ _ _ m -> m
    _ -> error "fileOutEntryMethodDefinitions"

-- | (ClassDeclaration, ClassComment, ClassInitializer?, [ClassMethodsFor], [InstanceMethodsFor])
type FileOutClassDef = (FileOutEntry, FileOutEntry, Maybe FileOutEntry, [FileOutEntry], [FileOutEntry])

{- | Derive FileOutClassDef for named class.
     Requires that there is a ClassDeclaration.
     Will generate a default ClassComment if none is present.
-}
fileOutLibraryClassDef :: FileOutLibrary -> St.Identifier -> Maybe FileOutClassDef
fileOutLibraryClassDef e x =
  let r = filter ((== x) . fileOutEntryClass) e
      cd = filter ((== "ClassDeclaration") . fileOutEntryType) r
      cc = filter ((== "ClassComment") . fileOutEntryType) r
      ci = filter ((== "ClassInitializer") . fileOutEntryType) r
      cm = filter ((== "ClassMethodsFor") . fileOutEntryType) r
      im = filter ((== "InstanceMethodsFor") . fileOutEntryType) r
      unlist l = if length l == 1 then Just (List.head_err l) else Nothing
  in case (unlist cd) of
      Just cd1 -> Just (cd1, fromMaybe (FileOutClassComment x "No comment") (unlist cc), unlist ci, cm, im)
      _ -> Nothing

fileOutLibraryClassDefOrError :: FileOutLibrary -> St.Identifier -> FileOutClassDef
fileOutLibraryClassDefOrError e = fromMaybe (error "fileOutLibraryClassDef: not class definition") . fileOutLibraryClassDef e

-- | Select all of the class and instance method entries for the named class.
fileOutLibraryClassMethodEntries :: FileOutLibrary -> St.Identifier -> [FileOutEntry]
fileOutLibraryClassMethodEntries e x =
  let r = filter ((== x) . fileOutEntryClass) e
      cm = filter ((== "ClassMethodsFor") . fileOutEntryType) r
      im = filter ((== "InstanceMethodsFor") . fileOutEntryType) r
  in cm ++ im

-- | Select all of the class and instance method definitions for the named class.
fileOutLibraryClassMethods :: FileOutLibrary -> St.Identifier -> [St.MethodDefinition]
fileOutLibraryClassMethods e = concatMap fileOutEntryMethodDefinitions . fileOutLibraryClassMethodEntries e

fileOutEntryClassComment :: FileOutEntry -> String
fileOutEntryClassComment cc =
  case cc of
    FileOutClassComment _className comment -> comment
    _ -> error "fileOutEntryClassComment"

-- | Translate FileOutClassDef to Ansi ClassDefinition.
fileOutClassDefToClassDefinition :: FileOutClassDef -> St.ClassDefinition
fileOutClassDefToClassDefinition (cd, cc, ci, cm, im) =
  let FileOutClassDeclaration superclassName className indexable instanceVariables classVariables poolDictionaries category = cd
      comment = fileOutEntryClassComment cc
      importedPoolNames = poolDictionaries
      instanceMethods = concatMap fileOutEntryMethodDefinitions im
      classMethods = concatMap fileOutEntryMethodDefinitions cm
  in St.ClassDefinition
      className
      (if superclassName == "nil" then Nothing else Just superclassName)
      (St.InstanceState indexable [])
      instanceVariables
      classVariables
      importedPoolNames
      instanceMethods
      classMethods
      (fmap (const (St.standardClassInitializerDefinition className)) ci)
      (Just category)
      (Just comment)

fileOutLibraryClassDefinition :: FileOutLibrary -> St.Identifier -> Maybe St.ClassDefinition
fileOutLibraryClassDefinition e = fmap fileOutClassDefToClassDefinition . fileOutLibraryClassDef e

fileOutLibraryClassDefinitionOrError :: FileOutLibrary -> St.Identifier -> St.ClassDefinition
fileOutLibraryClassDefinitionOrError fo = maybe (error "fileOutLibraryClassDefinition") id . fileOutLibraryClassDefinition fo

-- * Class definition printer

fileOutMethodDefinitionsForEntry :: FileOutEntry -> String
fileOutMethodDefinitionsForEntry s =
  case s of
    FileOutMethodsFor className isClassMethod category methodList ->
      let selectedMethods = filter (\m -> St.methodCategoryRequired m == category) methodList
      in unlines
          [ printf "!%s%s methodsFor: '%s'!" className (if isClassMethod then " class" else "") category
          , unlines (map (\method -> St.methodDefinition_pp method ++ "!") selectedMethods) ++ "!"
          ]
    _ -> error "fileOutMethodDefinitionsForEntry"

-- | Print the methodsFor: section of a FileOut for the class or instance methods for the named category.
fileOutMethodDefinitionsFor :: Bool -> St.ClassDefinition -> St.MethodCategory -> String
fileOutMethodDefinitionsFor isClassMethod classDef category =
  fileOutMethodDefinitionsForEntry
    ( FileOutMethodsFor
        (St.className classDef)
        isClassMethod
        category
        (if isClassMethod then St.classMethods classDef else St.instanceMethods classDef)
    )

-- | Print a methodsFor: section of a FileOut for a single method.
fileOutMethodDefinition :: St.MethodDefinition -> String
fileOutMethodDefinition md =
  fileOutMethodDefinitionsForEntry
    ( FileOutMethodsFor
        (St.methodClassName md)
        (St.isClassMethod md)
        (St.methodCategoryRequired md)
        [md]
    )

-- | Print the fileout class instantiation section for a class definition.
fileOutClassInstantiation :: St.ClassDefinition -> String
fileOutClassInstantiation cl =
  unlines
    [ printf "%s %s #%s" (fromMaybe "nil" (St.superclassName cl)) (St.instanceStateToSubclassKind (St.instanceState cl)) (St.className cl)
    , printf "  instanceVariableNames: '%s'" (unwords (St.classInstanceVariableNames cl))
    , printf "  classVariableNames: '%s'" (unwords (St.classVariableNames cl))
    , printf "  poolDictionaries: '%s'" (unwords (St.importedPoolNames cl))
    , printf "  category: '%s'!" (fromMaybe "" (St.classCategory cl))
    ]

-- | Print the fileout comment: for a class definition.
fileOutClassComment :: St.ClassDefinition -> String
fileOutClassComment cl = printf "%s comment: '%s'!" (St.className cl) (St.quoteQuote (fromMaybe "" (St.classComment cl)))

{- | This is printed if the class has a class initialize method.
     The FileOut parser reads standard initialize instructions, and stores a standard initializer at the class definition.
-}
fileOutClassInitializer :: St.ClassDefinition -> String
fileOutClassInitializer cd =
  case St.classInitializer cd of
    Just (St.InitializerDefinition Nothing Nothing (Just (St.StatementsExpression e Nothing))) -> St.expression_pp e ++ "!"
    _ -> ""

fileOutClassDefinition :: St.ClassDefinition -> String
fileOutClassDefinition cl =
  let im = St.classInstanceMethodCategories cl
      cm = St.classClassMethodCategories cl
  in unlines
      [ fileOutClassInstantiation cl
      , fileOutClassComment cl
      , ""
      , unlines (map (fileOutMethodDefinitionsFor False cl) im)
      , unlines (map (fileOutMethodDefinitionsFor True cl) cm)
      , fileOutClassInitializer cl
      ]

-- * Class extensions printer

fileOutClassMethodExtensions :: St.Identifier -> [FileOutEntry] -> String
fileOutClassMethodExtensions className =
  unlines
    . map fileOutMethodDefinitionsForEntry
    . filter (isFileOutMethodsForClass className)

-- * Parser

{- | Chunks are delimited by !.

> St.stParse chunkDelimiter "!" == '!'
> St.stParse chunkDelimiter "!!" -- fail
> St.stParse chunkDelimiter "!C" -- fail
-}
chunkDelimiter :: St.P Char
chunkDelimiter = P.char '!' St.>>~ P.notFollowedBy (P.letter P.<|> P.char '!') St.>>~ P.many P.space

{- | ! is escaped as !!.

> St.stParse quotedExclamationPoint "!!" == '!'
> St.stParse quotedExclamationPoint "!" -- fail
-}
quotedExclamationPoint :: St.P Char
quotedExclamationPoint = P.try (P.char '!' >> P.char '!')

-- | Any char that is not an unquoted !.
allowedChunkChar :: St.P Char
allowedChunkChar = quotedExclamationPoint P.<|> P.noneOf ['!']

{- | Sequence of allowedChunkChar.

>>> St.stParse chunkText "Any text!"
"Any text"

>>> St.stParse chunkText "Any text with quoted exclamation points also!!!"
"Any text with quoted exclamation points also!"

>>> St.stParse chunkText "Any text with $!! character literals!"
"Any text with $! character literals"

>>> St.stParse chunkText "Any text with 'string literals!!' literals!"
"Any text with 'string literals!' literals"
-}
chunkText :: St.P String
chunkText = P.many1 allowedChunkChar St.>>~ chunkDelimiter

{- | A chunk that has only whitespace.

>>> St.stParse emptyChunk " !"
" "

> St.stParse emptyChunk " x !" -- error
-}
emptyChunk :: St.P Whitespace
emptyChunk = P.many P.space St.>>~ chunkDelimiter

{- | An ordinary chunk.  Does not delete leading whitespace.

>>> St.stParse nonEmptyChunk " x !"
" x "

>>> St.stParse nonEmptyChunk " x!! !"
" x! "

> St.stParse nonEmptyChunk " !" -- error

>>> St.stParse (P.many1 nonEmptyChunk) " x ! y ! z!! !"
[" x ","y ","z! "]
-}
nonEmptyChunk :: St.P Chunk
nonEmptyChunk = (P.try emptyChunk >> P.unexpected "emptyChunk") P.<|> chunkText

-- | A sequence of one or more non empty chunks followed by an empty chunk.
nonEmptyChunkSequence :: St.P [Chunk]
nonEmptyChunkSequence = P.many1 (P.try nonEmptyChunk) St.>>~ emptyChunk

-- | A sequence of one or more non empty chunks followed by an empty chunk.
chunkSequence :: St.P [Chunk]
chunkSequence = P.many (P.try nonEmptyChunk) St.>>~ emptyChunk

{- | A Reader.

>>> St.stParse reader "!reader methodsFor: 'a category'!"
"reader methodsFor: 'a category'"
-}
reader :: St.P Reader
reader = P.char '!' >> nonEmptyChunk

{- | Parser for an Eval segment, a single non-empty chunk.

>>> let p = St.stParse fileOutEvalSegment
>>> p " x ! "
FileOutEvalSegment " x "

>>> p "X x!\n!Y y! !"
FileOutEvalSegment "X x"
-}
fileOutEvalSegment :: St.P FileOutSegment
fileOutEvalSegment = fmap FileOutEvalSegment nonEmptyChunk

{- | The reader segments allowed consist of identifiers, keywords, literal strings and integers.
The integer (at commentStamp:prior:) is converted to a string.

>>> St.stParse readerQuotedWords "reader methodsFor: 'a category'"
["reader","methodsFor:","a category"]

>>> St.stParse readerQuotedWords "reader commentStamp: 'a comment' prior: 0"
["reader","commentStamp:","a comment","prior:","0"]
-}
readerQuotedWords :: St.P [String]
readerQuotedWords = P.many1 (P.choice [P.try St.quotedString, P.try St.keyword, St.identifier, fmap show St.integer])

-- | Run readerQuotedWords parser.
readerWords :: String -> [String]
readerWords = St.stParse readerQuotedWords

{- | Parser for Reader segment.  Does not delete leading spaces.

>>> let p = St.stParse fileOutReaderSegment
>>> p "!reader methodsFor: 'a category'! chunk! !"
FileOutReaderSegment "reader methodsFor: 'a category'" ["chunk"]

>>> p "!reader methodsFor: 'a category' stamp: 'initials and time'! chunk! !"
FileOutReaderSegment "reader methodsFor: 'a category' stamp: 'initials and time'" ["chunk"]

>>> p "!reader class methodsFor: 'a category'! chunk! !"
FileOutReaderSegment "reader class methodsFor: 'a category'" ["chunk"]

>>> p "!reader class methodsFor: 'a category' stamp: ''! chunk! !"
FileOutReaderSegment "reader class methodsFor: 'a category' stamp: ''" ["chunk"]

>>> p "!reader class methodsFor: 'a category'! !" -- allow empty methods sequence
FileOutReaderSegment "reader class methodsFor: 'a category'" []

>>> p "!reader commentStamp: 'a comment' prior: 0 ! chunk!"
FileOutReaderSegment "reader commentStamp: 'a comment' prior: 0 " ["chunk"]

> p "!p! q! !" -- fail on p

> p "!Z z! c! !" -- fail on Z
-}
fileOutReaderSegment :: St.P FileOutSegment
fileOutReaderSegment = do
  r <- reader
  s <- case readerWords r of
    _ : "commentStamp:" : _ : "prior:" : _ : [] -> fmap return nonEmptyChunk
    _ : "class" : "methodsFor:" : _ : [] -> chunkSequence
    _ : "class" : "methodsFor:" : _ : "stamp:" : _ : [] -> chunkSequence
    _ : "methodsFor:" : _ : [] -> chunkSequence
    _ : "methodsFor:" : _ : "stamp:" : _ : [] -> chunkSequence
    w -> P.unexpected ("fileOutReaderSegment: " ++ show w)
  return (FileOutReaderSegment r s)

{- | Parser for FileOut segment.

>>> let p = St.stParse fileOutSegment
>>> p "'A string chunk!!'!"
FileOutEvalSegment "'A string chunk!'"

>>> p "\"A comment chunk!!\"!"
FileOutEvalSegment "\"A comment chunk!\""

>>> p "!reader methodsFor: 'a category'! chunk one!!! chunk two!!! !" -- quoted !
FileOutReaderSegment "reader methodsFor: 'a category'" ["chunk one!","chunk two!"]

> p "!p ! q ! !" -- fail on p
-}
fileOutSegment :: St.P FileOutSegment
fileOutSegment = P.try fileOutReaderSegment P.<|> fileOutEvalSegment

{- | Parser for FileOut.

>>> let p = St.stParse fileOut
>>> p "x! y! !"
[FileOutEvalSegment "x",FileOutEvalSegment "y"]

>>> p "Object subclass: #UndefinedObject instanceVariableNames: '' classVariableNames: '' category: 'Kernel-Objects'!"
[FileOutEvalSegment "Object subclass: #UndefinedObject instanceVariableNames: '' classVariableNames: '' category: 'Kernel-Objects'"]

>>> p "ArrayedCollection class instanceVariableNames: ''! !ArrayedCollection class methodsFor: '' stamp: ''! m ^ nil! !"
[FileOutEvalSegment "ArrayedCollection class instanceVariableNames: ''",FileOutReaderSegment "ArrayedCollection class methodsFor: '' stamp: ''" ["m ^ nil"]]

>>> p "!C methodsFor: 'some category name text'!\naUnaryMethod ^nil! !"
[FileOutReaderSegment "C methodsFor: 'some category name text'" ["aUnaryMethod ^nil"]]

> p "!p ! q ! ! x ! !" -- fail on p

> p "x ! y ! !p ! q ! ! z ! !" -- fail on p

> p "X x!\nY y!\n!Z z! c! !" -- fail on Z
-}
fileOut :: St.P FileOut
fileOut = P.many1 (P.try fileOutSegment) St.>>~ P.optional emptyChunk

{- | Run fileOut parser.

> f0:_ = parseFileOut "File subclass: #AltoFile\n\tinstanceVariableNames: 'writeTime pageAddresses leader '\n\tclassVariableNames: ''\n\tpoolDictionaries: 'AltoFilePool '\n\tcategory: 'Files-Xerox Alto'!"
> segmentBasicExpression f0

> f0:_ = parseFileOut "ArrayedCollection class instanceVariableNames: ''!"
> segmentBasicExpression f0
-}
parseFileOut :: String -> FileOut
parseFileOut = St.stParse fileOut

{- | Read named file and run fileOut parser.

> load fn = loadFileOut ("/home/rohan/sw/stsc3/st/" ++ fn)
> load "SC3-Base.st"
> load "SC3-Core.st"
> load "SC3-Env.st"
> load "SC3-Event.st"
> load "SC3-Haskell.st"
> load "SC3-Math.st"
> load "SC3-Random.st"
> load "SC3-UGen.st"
> load "SC3-UGen-Composite.st"
-}
loadFileOut :: FilePath -> IO FileOut
loadFileOut = fmap parseFileOut . readFile

-- * Reader

segmentBasicExpression :: FileOutSegment -> Maybe St.BasicExpression
segmentBasicExpression fo =
  case fo of
    FileOutEvalSegment txt -> Just (St.stParseInitial St.basicExpression txt)
    _ -> Nothing

evalSegmentClassDeclaration :: FileOutSegment -> Maybe FileOutEntry
evalSegmentClassDeclaration fo =
  case fo of
    FileOutEvalSegment txt -> parseFileOutClassDeclaration (St.stParseInitial St.basicExpression txt)
    _ -> Nothing

-- | Parse a subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: message send.
parseFileOutClassDeclaration :: St.BasicExpression -> Maybe FileOutEntry
parseFileOutClassDeclaration e =
  case e of
    St.BasicExpression
      (St.PrimaryIdentifier sp)
      ( Just
          ( St.MessagesKeyword
              ( St.KeywordMessage
                  [ (subclassKind, St.KeywordArgument (St.PrimaryLiteral (St.SelectorLiteral (St.UnarySelector cl))) Nothing Nothing)
                    , ("instanceVariableNames:", St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral ivar)) Nothing Nothing)
                    , ("classVariableNames:", St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral cvar)) Nothing Nothing)
                    , ("poolDictionaries:", St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral pd)) Nothing Nothing)
                    , ("category:", St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral cat)) Nothing Nothing)
                    ]
                )
            )
        )
      Nothing ->
        Just (FileOutClassDeclaration sp cl (St.subclassKindToIndexable subclassKind) (words ivar) (words cvar) (words pd) cat)
    _ -> Nothing

evalClassClassInstanceVariables :: FileOutSegment -> Maybe FileOutEntry
evalClassClassInstanceVariables fo =
  case fo of
    FileOutEvalSegment txt -> parseFileOutClassClassInstanceVariables (St.stParseInitial St.basicExpression txt)
    _ -> Nothing

parseFileOutClassClassInstanceVariables :: St.BasicExpression -> Maybe FileOutEntry
parseFileOutClassClassInstanceVariables e =
  case e of
    St.BasicExpression
      (St.PrimaryIdentifier cl)
      ( Just
          ( St.MessagesUnary
              [St.UnaryMessage "class"]
              Nothing
              (Just (St.KeywordMessage [("instanceVariableNames:", St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral nm)) Nothing Nothing)]))
            )
        )
      Nothing -> Just (FileOutClassClassInstanceVariables cl (words nm))
    _ -> Nothing

evalSegmentClassComment :: FileOutSegment -> Maybe FileOutEntry
evalSegmentClassComment fo =
  case fo of
    FileOutEvalSegment txt -> parseFileOutClassComment (St.stParseInitial St.basicExpression txt)
    _ -> Nothing

-- | Parse a comment: message send.
parseFileOutClassComment :: St.BasicExpression -> Maybe FileOutEntry
parseFileOutClassComment e =
  case e of
    St.BasicExpression
      (St.PrimaryIdentifier cl)
      ( Just
          ( St.MessagesKeyword
              ( St.KeywordMessage
                  [("comment:", St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral comment)) Nothing Nothing)]
                )
            )
        )
      Nothing ->
        Just (FileOutClassComment cl comment)
    _ -> Nothing

evalSegmentClassInitializer :: FileOutSegment -> Maybe FileOutEntry
evalSegmentClassInitializer fo =
  case fo of
    FileOutEvalSegment txt -> parseFileOutClassInitializer (St.stParseInitial St.basicExpression txt)
    _ -> Nothing

-- | Parse a class initialize message send.
parseFileOutClassInitializer :: St.BasicExpression -> Maybe FileOutEntry
parseFileOutClassInitializer e =
  case e of
    St.BasicExpression
      (St.PrimaryIdentifier cl)
      (Just (St.MessagesUnary [St.UnaryMessage "initialize"] Nothing Nothing))
      Nothing -> Just (FileOutClassInitializer cl)
    _ -> Nothing

parseMethodsForMethod :: Bool -> St.Identifier -> St.MethodCategory -> String -> St.MethodDefinition
parseMethodsForMethod isClassMethod classname category txt =
  (St.stParse (St.methodDefinition (Just txt) (classname ++ if isClassMethod then " class" else "", isClassMethod)) txt)
    { St.methodCategory = Just category
    }

{- | Parse a methodsFor: message send.
The receiver is either the name of a class, the class of such.
Allow but ignore trailing stamp: keyword.

> readerSegmentMethodDefinitions (St.stParse fileOutSegment "!C methodsFor: 'the category text'!\nm ^nil! !")
-}
readerSegmentMethodDefinitions :: FileOutSegment -> Maybe FileOutEntry
readerSegmentMethodDefinitions fo =
  case fo of
    FileOutReaderSegment r c ->
      case readerWords r of
        classname : "methodsFor:" : category : _ ->
          Just (FileOutMethodsFor classname False category (map (parseMethodsForMethod False classname category) c))
        classname : "class" : "methodsFor:" : category : _ ->
          Just (FileOutMethodsFor classname True category (map (parseMethodsForMethod True classname category) c))
        _ -> Nothing
    _ -> Nothing

trySequence :: [t -> Maybe r] -> t -> Maybe r
trySequence sq x =
  case sq of
    [] -> Nothing
    f : sq' -> case f x of
      Nothing -> trySequence sq' x
      Just r -> Just r

evalFileOutSegmentMaybe :: FileOutSegment -> Maybe FileOutEntry
evalFileOutSegmentMaybe =
  trySequence
    [ evalSegmentClassDeclaration
    , evalSegmentClassComment
    , evalSegmentClassInitializer
    , readerSegmentMethodDefinitions
    , evalClassClassInstanceVariables
    ]

evalFileOutSegment :: FileOutSegment -> Either FileOutEntry FileOutSegment
evalFileOutSegment s = maybe (Right s) Left (evalFileOutSegmentMaybe s)

-- | Predicate to decide if a segment is a comment.
isComment :: FileOutSegment -> Bool
isComment fo =
  case fo of
    FileOutEvalSegment txt -> List.head_err txt == '"' && last txt == '"'
    _ -> False

-- | Predicate to decide if a segment is a string.
isString :: FileOutSegment -> Bool
isString fo =
  case fo of
    FileOutEvalSegment txt -> List.head_err txt == '\'' && last txt == '\''
    _ -> False

evalFileOut :: FileOut -> [Either FileOutEntry FileOutSegment]
evalFileOut = map evalFileOutSegment . filter (\x -> not (isComment x || isString x))

-- | File out library, ignoring any segments that cannot be parsed.
fileOutLibraryPartial :: FileOut -> FileOutLibrary
fileOutLibraryPartial = lefts . evalFileOut

fileOutLibrary :: FileOut -> FileOutLibrary
fileOutLibrary = map (either id (\e -> error (show ("evalFileOut: parse failed: ", e)))) . evalFileOut

fileOutUnparsedSegments :: FileOut -> [FileOutSegment]
fileOutUnparsedSegments = rights . evalFileOut

-- * Load class file

fileOutLoadPartial :: FilePath -> IO FileOutLibrary
fileOutLoadPartial = fmap fileOutLibraryPartial . loadFileOut

fileOutLoad :: FilePath -> IO FileOutLibrary
fileOutLoad = fmap fileOutLibrary . loadFileOut

{- | Load singular class definition from fileout, else error.

fo = file-out, fn = file-name, cd = class-definition, cn = class-name
-}
fileOutLoadClassDefinitionFile :: FilePath -> IO St.ClassDefinition
fileOutLoadClassDefinitionFile fn = do
  fo <- loadFileOut fn
  let ent = fileOutLibraryPartial fo
  case fileOutLibraryClassesDefined ent of
    [cn] ->
      case fileOutLibraryClassDefinition ent cn of
        Just cd -> return cd
        Nothing -> error "fileOutLoadClassFile: not class definition?"
    _ -> error "fileOutLoadClassFile: not singular class file?"

{- | Load a FileOut containing method definitions for a single class, else error.

(cl, mth) <- fileOutLoadClassExtensionFile "/home/rohan/sw/stsc3/st/Array.ext.st"
(cl, length mth) == ("Array",11)
-}
fileOutLoadClassExtensionFile :: FilePath -> IO (St.Identifier, [St.MethodDefinition])
fileOutLoadClassExtensionFile fn = do
  fo <- loadFileOut fn
  let ent = fileOutLibraryPartial fo
      mth = concatMap fileOutEntryMethodDefinitions ent
      cls = nub (map St.methodClassName mth)
  case cls of
    [nm] -> return (nm, mth)
    _ -> error "fileOutLoadClassExtensionFile: methods not for singular class?"

-- * Summary

fileOutLibraryPrintSummary :: FileOutLibrary -> IO ()
fileOutLibraryPrintSummary lib = do
  let cls = fileOutLibraryClassSet lib
      def = fileOutLibraryClassesDefined lib
      ext = fileOutLibraryClassesExtended lib
  print ("#entries", length lib)
  print ("#classes", length cls)
  print ("#classes defined", length def)
  print ("#classes extended", length ext)
  print ("#classes unknown", length cls - length ext - length def)
