{- | FileOut Code Format

<http://wiki.squeak.org/squeak/1105>

A fileout consists of a sequence of code segments called "chunks" separated by a ! character.

Any ! character occurring within the code segment must be quoted as !!

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
data FileOutEntry =
    FileOutClassDeclaration St.Identifier St.Identifier St.Indexable [St.Identifier] [St.Identifier] [St.Identifier] String
  -- ^ superclass, class, subclass kind, [instance variable], [class variable], [pool dictionary], category
  | FileOutClassComment St.Identifier String
  -- ^ class, comment
  | FileOutClassInitializer St.Identifier
  -- ^ class
  | FileOutMethodsFor St.Identifier Bool [St.MethodDefinition]
  -- ^ class, classmethods?, [methods]
  deriving (Eq, Show)

isFileOutClassDeclaration :: FileOutEntry -> Bool
isFileOutClassDeclaration e =
  case e of
    FileOutClassDeclaration {} -> True
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
    FileOutMethodsFor x _ _ -> x

{- | The set of all classes that have entries.
     This includes classes that have method definitions but no declarations.
-}
fileOutEntryClassSet :: [FileOutEntry] -> [St.Identifier]
fileOutEntryClassSet = nub . sort . map fileOutEntryClass

-- | The set of classes that are defined (declared).
fileOutEntryClassesDefined :: [FileOutEntry] -> [St.Identifier]
fileOutEntryClassesDefined = nub . sort . map fileOutEntryClass . filter isFileOutClassDeclaration

-- | The set of classes that are extended (have method definitions but are not declared).
fileOutEntryClassesExtended :: [FileOutEntry] -> [St.Identifier]
fileOutEntryClassesExtended l =
  let def = fileOutEntryClassesDefined l
      ent = fileOutEntryClassSet l
  in ent \\ def

-- | Type string of entry, for pretty printing.
fileOutEntryType :: FileOutEntry -> String
fileOutEntryType e =
  case e of
    FileOutClassDeclaration {} -> "ClassDeclaration"
    FileOutClassComment {} -> "ClassComment"
    FileOutClassInitializer {} -> "ClassInitializer"
    FileOutMethodsFor _ c _ -> (if c then "Class" else "Instance") ++ "MethodsFor"

-- | Method definitions of entry, or error if not a MethodsFor entry.
fileOutEntryMethodDefinitions :: FileOutEntry -> [St.MethodDefinition]
fileOutEntryMethodDefinitions e =
  case e of
    FileOutMethodsFor _ _ m -> m
    _ -> error "fileOutEntryMethodDefinitions"

-- | (ClassDeclaration,ClassComment,[ClassMethodsFor],[InstanceMethodsFor])
type FileOutClassDef = (FileOutEntry,FileOutEntry,Maybe FileOutEntry,[FileOutEntry],[FileOutEntry])

{- | Derive FileOutClassDef for named class.
     Requires that there is a ClassDeclaration.
     Will generate a default ClassComment if none is present.
-}
fileOutEntryClassDef :: [FileOutEntry] -> St.Identifier -> Maybe FileOutClassDef
fileOutEntryClassDef e x =
  let r = filter ((== x) . fileOutEntryClass) e
      cd = filter ((== "ClassDeclaration") . fileOutEntryType) r
      cc = filter ((== "ClassComment") . fileOutEntryType) r
      ci = filter ((== "ClassInitializer") . fileOutEntryType) r
      cm = filter ((== "ClassMethodsFor") . fileOutEntryType) r
      im = filter ((== "InstanceMethodsFor") . fileOutEntryType) r
      unlist l = if length l == 1 then Just (head l) else Nothing
  in case (unlist cd) of
       Just cd1 -> Just (cd1,fromMaybe (FileOutClassComment x "No comment") (unlist cc), unlist ci,cm,im)
       _ -> Nothing

fileOutEntryClassDefOrError :: [FileOutEntry] -> St.Identifier -> FileOutClassDef
fileOutEntryClassDefOrError e = fromMaybe (error "fileOutEntryClassDef: not class definition") . fileOutEntryClassDef e

-- | Translate FileOutClassDef to Ansi ClassDefinition.
fileOutClassDefToClassDefinition :: FileOutClassDef -> St.ClassDefinition
fileOutClassDefToClassDefinition (cd,cc,_ci,cm,im) =
  let FileOutClassDeclaration superclassName className indexable instanceVariables classVariables poolDictionaries category = cd
      FileOutClassComment _className comment = cc
      importedPoolNames = poolDictionaries
      instanceMethods = concatMap fileOutEntryMethodDefinitions im
      classMethods = concatMap fileOutEntryMethodDefinitions cm
  in St.ClassDefinition
     className
     (Just superclassName)
     (St.InstanceState indexable []) -- ?
     instanceVariables
     classVariables
     importedPoolNames
     instanceMethods
     classMethods
     Nothing
     (Just category)
     (Just comment)

fileOutEntryClassDefinition :: [FileOutEntry] -> St.Identifier -> Maybe St.ClassDefinition
fileOutEntryClassDefinition e = fmap fileOutClassDefToClassDefinition . fileOutEntryClassDef e

-- * Class definition printer

fileOutMethodDefinitionsFor :: Bool -> St.ClassDefinition -> String -> String
fileOutMethodDefinitionsFor isClassMethod cl cat =
  let mth = filter (\m -> St.methodCategoryRequired m == cat) (if isClassMethod then St.classMethods cl else St.instanceMethods cl)
  in unlines
     [printf "!%s %s methodsFor: '%s'!" (St.className cl) (if isClassMethod then "class" else "") cat
     ,unlines (map (\m -> St.methodDefinition_pp m ++ "!") mth) ++ "!"]

fileOutClassComment :: St.ClassDefinition -> String
fileOutClassComment cl = printf "%s comment: '%s'!\n" (St.className cl) (fromMaybe "" (St.classComment cl))

fileOutClassInstantiation :: St.ClassDefinition -> String
fileOutClassInstantiation cl =
  unlines
  [printf "%s %s #%s" (fromMaybe "Object" (St.superclassName cl)) (St.instanceStateToSubclassKind (St.instanceState cl)) (St.className cl)
  ,printf "  instanceVariableNames: '%s'" (unwords (St.classInstanceVariableNames cl))
  ,printf "  classVariableNames: '%s'" (unwords (St.classVariableNames cl))
  ,printf "  poolDictionaries: '%s'" (unwords (St.importedPoolNames cl))
  ,printf "  category: '%s'!" (fromMaybe "" (St.classCategory cl))]

{- | This is printed if the class has a class initialize method.
     Although the FileOut parser reads the initialize instruction it's presence is not stored at the class definition.
-}
fileOutClassInitializer :: St.ClassDefinition -> String
fileOutClassInitializer cl = if St.classHasClassInitializer cl then printf "%s initialize!" (St.className cl) else ""

fileOutClassDefinition :: St.ClassDefinition -> String
fileOutClassDefinition cl =
  let im = St.classInstanceMethodCategories cl
      cm = St.classClassMethodCategories cl
  in unlines
     [fileOutClassInstantiation cl
     ,fileOutClassComment cl
     ,unlines (map (fileOutMethodDefinitionsFor False cl) im)
     ,unlines (map (fileOutMethodDefinitionsFor True cl) cm)
     ,fileOutClassInitializer cl]

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

> St.stParse chunkText "Any text"
> St.stParse chunkText "Any text with quoted exclamation points also!!"
-}
chunkText :: St.P String
chunkText = P.many1 allowedChunkChar

{- | A chunk that has only whitespace.

> St.stParse emptyChunk " !" == " "
> St.stParse emptyChunk " x !" -- error
-}
emptyChunk :: St.P Whitespace
emptyChunk = P.many P.space St.>>~ chunkDelimiter

{- | An ordinary chunk.  Does not delete leading whitespace.

> St.stParse nonEmptyChunk " x !" == " x "
> St.stParse nonEmptyChunk " x!! !" == " x! "
> St.stParse nonEmptyChunk " !" -- error
> St.stParse (P.many1 nonEmptyChunk) " x ! y ! z!! !" == [" x ","y ","z! "]
-}
nonEmptyChunk :: St.P Chunk
nonEmptyChunk =
  ((P.try emptyChunk >> P.unexpected "emptyChunk") P.<|>
   (P.many1 allowedChunkChar St.>>~ chunkDelimiter))

-- | A sequence of one or more non empty chunks followed by an empty chunk.
nonEmptyChunkSequence :: St.P [Chunk]
nonEmptyChunkSequence = P.many1 (P.try nonEmptyChunk) St.>>~ emptyChunk

-- | A sequence of one or more non empty chunks followed by an empty chunk.
chunkSequence :: St.P [Chunk]
chunkSequence = P.many (P.try nonEmptyChunk) St.>>~ emptyChunk

{- | A Reader.

> St.stParse reader "!reader methodsFor: 'a category'!"
-}
reader :: St.P Reader
reader = P.char '!' >> nonEmptyChunk

{- | Parser for an Eval segment, a single non-empty chunk.

> p = St.stParse fileOutEvalSegment
> p " x ! " == FileOutEvalSegment " x "
> p "X x!\n!Y y! !" == FileOutEvalSegment "X x"
-}
fileOutEvalSegment :: St.P FileOutSegment
fileOutEvalSegment = fmap FileOutEvalSegment nonEmptyChunk

{- | The reader segments allowed consiste of identifiers, keywords and literal strings.

> St.stParse readerQuotedWords "reader methodsFor: 'a category'"
-}
readerQuotedWords :: St.P [String]
readerQuotedWords = P.many1 (P.choice [P.try St.quotedString, P.try St.keyword, St.identifier])

-- | Run readerQuotedWords parser.
readerWords :: String -> [String]
readerWords = St.stParse readerQuotedWords

{- | Parser for Reader segment.  Does not delete leading spaces.

> p = St.stParse fileOutReaderSegment
> p "!reader methodsFor: 'a category'! chunk! !"
> p "!reader class methodsFor: 'a category'! chunk! !"
> p "!reader class methodsFor: 'a category'! !" -- allow empty methods sequence
> p "!reader commentStamp: 'a comment'! chunk!"
> p "!p! q! !" -- fail on p
> p "!Z z! c! !" -- fail on Z
-}
fileOutReaderSegment :: St.P FileOutSegment
fileOutReaderSegment = do
  r <- reader
  s <- case readerWords r of
         _:"commentStamp:":_:[] -> fmap return nonEmptyChunk
         _:"class":"methodsFor:":_:[] -> chunkSequence
         _:"methodsFor:":_:[] -> chunkSequence
         _ -> P.unexpected ("fileOutReaderSegment: " ++ r)
  return (FileOutReaderSegment r s)

{- | Parser for FileOut segment.

> p = St.stParse fileOutSegment
> p "'A string chunk!!'!" == FileOutEvalSegment "'A string chunk!'"
> p "\"A comment chunk!!\"!" == FileOutEvalSegment "\"A comment chunk!\""
> p "!reader methodsFor: 'a category'! chunk one!!! chunk two!!! !" -- quoted !
> p "!p ! q ! !" -- fail on p
-}
fileOutSegment :: St.P FileOutSegment
fileOutSegment = P.try fileOutReaderSegment P.<|> fileOutEvalSegment

{- | Parser for FileOut.

> p = St.stParse fileOut
> p "x! y! !"
> p "Object subclass: #UndefinedObject instanceVariableNames: '' classVariableNames: '' category: 'Kernel-Objects'!"
> p "!C methodsFor: 'some category name text'!\naUnaryMethod ^nil! !"
> p "!p ! q ! ! x ! !" -- fail on p
> p "x ! y ! !p ! q ! ! z ! !" -- fail on p
> p "X x!\nY y!\n!Z z! c! !" -- fail on Z
-}
fileOut :: St.P FileOut
fileOut = P.many1 (P.try fileOutSegment) St.>>~ P.optional emptyChunk

{- | Run fileOut parser.

> f0:_ = parseFileOut "File subclass: #AltoFile\n\tinstanceVariableNames: 'writeTime pageAddresses leader '\n\tclassVariableNames: ''\n\tpoolDictionaries: 'AltoFilePool '\n\tcategory: 'Files-Xerox Alto'!"
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
      (Just (St.MessagesKeyword
              (St.KeywordMessage
                [(subclassKind,St.KeywordArgument (St.PrimaryLiteral (St.SelectorLiteral (St.UnarySelector cl))) Nothing Nothing)
                ,("instanceVariableNames:",St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral ivar)) Nothing Nothing)
                ,("classVariableNames:",St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral cvar)) Nothing Nothing)
                ,("poolDictionaries:",St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral pd)) Nothing Nothing)
                ,("category:",St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral cat)) Nothing Nothing)]))) Nothing ->
      Just (FileOutClassDeclaration sp cl (St.subclassKindToIndexable subclassKind) (words ivar) (words cvar) (words pd) cat)
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
      (Just (St.MessagesKeyword
              (St.KeywordMessage
                [("comment:",St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral comment)) Nothing Nothing)]))) Nothing ->
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
  (St.stParse (St.methodDefinition (Just txt) (classname ++ if isClassMethod then " class" else "")) txt)
  {St.methodCategory = Just category}

{- | Parse a methodsFor: message send.
     The receiver is either the name of a class, the class of such.

> readerSegmentMethodDefinitions (St.stParse fileOutSegment "!C methodsFor: 'the category text'!\nm ^nil! !")
-}
readerSegmentMethodDefinitions :: FileOutSegment -> Maybe FileOutEntry
readerSegmentMethodDefinitions fo =
  case fo of
    FileOutReaderSegment r c ->
      case readerWords r of
        classname:"methodsFor:":category:[] ->
          Just (FileOutMethodsFor classname False (map (parseMethodsForMethod False classname category) c))
        classname:"class":"methodsFor:":category:[] ->
          Just (FileOutMethodsFor classname True (map (parseMethodsForMethod True classname category) c))
        _ -> Nothing
    _ -> Nothing

trySequence :: [t -> Maybe r] -> t -> Maybe r
trySequence sq x =
  case sq of
    [] -> Nothing
    f:sq' -> case f x of
               Nothing -> trySequence sq' x
               Just r -> Just r

evalFileOutSegmentMaybe :: FileOutSegment -> Maybe FileOutEntry
evalFileOutSegmentMaybe =
  trySequence
  [evalSegmentClassDeclaration
  ,evalSegmentClassComment
  ,evalSegmentClassInitializer
  ,readerSegmentMethodDefinitions]

evalFileOutSegment :: FileOutSegment -> Either FileOutEntry FileOutSegment
evalFileOutSegment s = maybe (Right s) Left (evalFileOutSegmentMaybe s)

-- | Predicate to decide if a segment is a comment.
isComment :: FileOutSegment -> Bool
isComment fo =
  case fo of
    FileOutEvalSegment txt -> head txt == '"' && last txt == '"'
    _ -> False

-- | Predicate to decide if a segment is a string.
isString :: FileOutSegment -> Bool
isString fo =
  case fo of
    FileOutEvalSegment txt -> head txt == '\'' && last txt == '\''
    _ -> False

evalFileOut :: FileOut -> [Either FileOutEntry FileOutSegment]
evalFileOut = map evalFileOutSegment . filter (\x -> not (isComment x || isString x))

evalFileOutSubset :: FileOut -> [FileOutEntry]
evalFileOutSubset = lefts . evalFileOut

evalFileOutOrError :: FileOut -> [FileOutEntry]
evalFileOutOrError = map (either id (\e -> error (show ("evalFileOut: parse failed: ", e)))) . evalFileOut
