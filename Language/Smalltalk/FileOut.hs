{- | FileOut Code Format

<http://wiki.squeak.org/squeak/1105>

A fileout consists of a sequence of code segments called "chunks" separated by a ! character.

Any ! character occurring within the code segment must be quoted as !!

An empty chunk consisting of one or more whitespace characters terminates the sequence.

These sequence takes one of two forms:

  <chunk>!<chunk>! ... !<whitespace>!
  !<reader>!<stream>!

The parser here accepts only a subset of the second form, that required to add methods to classes.
It assumes that stream is a sequence of chunks ending with an empty chunk.

-}
module Language.Smalltalk.FileOut where

import Data.Maybe {- base -}

import qualified Text.Parsec as P {- parsec -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

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
    FileOutClassDeclaration St.Identifier St.Identifier [String] [String] String
  -- ^ (superclass,class,instance variables,class variables,category)
  | FileOutClassComment St.Identifier String
  -- ^ (class,comment)
  | FileOutMethodsFor St.Identifier Bool [St.MethodDefinition]
  -- ^ (class,classmethods?,[methods])
  deriving (Eq, Show)

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
chunkSequence :: St.P [Chunk]
chunkSequence = P.many1 (P.try nonEmptyChunk) St.>>~ emptyChunk

{- | A Reader.

> St.stParse reader "!reader method: argument!"
-}
reader :: St.P Reader
reader = P.char '!' >> nonEmptyChunk

{- | Parser for an Eval segment, a single non-empty chunk.

> St.stParse fileOutEvalSegment " x ! " == FileOutEvalSegment " x "
-}
fileOutEvalSegment :: St.P FileOutSegment
fileOutEvalSegment = fmap FileOutEvalSegment nonEmptyChunk

{- | Parser for Reader segment.  Does not delete leading spaces.

> St.stParse fileOutReaderSegment "!reader method: arg! chunk! !"
> St.stParse fileOutReaderSegment "!p! q! !"
-}
fileOutReaderSegment :: St.P FileOutSegment
fileOutReaderSegment = do
  r <- reader
  s <- chunkSequence
  return (FileOutReaderSegment r s)

{- | Parser for FileOut segment.

> p = St.stParse fileOutSegment
> p "'A string chunk!!'!" == FileOutEvalSegment "'A string chunk!'"
> p "\"A comment chunk!!\"!" == FileOutEvalSegment "\"A comment chunk!\""
> p "!reader method: arg! chunk one!!! chunk two!!! !"
> p "!p ! q ! !"
-}
fileOutSegment :: St.P FileOutSegment
fileOutSegment = P.try fileOutReaderSegment P.<|> fileOutEvalSegment

{- | Parser for FileOut.

> p = St.stParse fileOut
> p "x! y! !"
> p "Object subclass: #UndefinedObject instanceVariableNames: '' classVariableNames: '' category: 'Kernel-Objects'! !"
> p "!p ! q ! ! x ! !"
> p "x ! y ! !p ! q ! ! z ! !"
-}
fileOut :: St.P FileOut
fileOut = P.many1 (P.try fileOutSegment) St.>>~ P.optional emptyChunk

-- | Run fileOut parser.
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

evalSegmentClassDeclaration :: FileOutSegment -> Maybe FileOutEntry
evalSegmentClassDeclaration fo =
  case fo of
    FileOutEvalSegment txt -> parseFileOutClassDeclaration (St.stParseInitial St.basicExpression txt)
    _ -> Nothing

parseFileOutClassDeclaration :: St.BasicExpression -> Maybe FileOutEntry
parseFileOutClassDeclaration e =
  case e of
    St.BasicExpression
      (St.PrimaryIdentifier sp)
      (Just (St.MessagesKeyword
              (St.KeywordMessage
                [("subclass:",St.KeywordArgument (St.PrimaryLiteral (St.SelectorLiteral (St.UnarySelector cl))) Nothing Nothing)
                ,("instanceVariableNames:",St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral ivar)) Nothing Nothing)
                ,("classVariableNames:",St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral cvar)) Nothing Nothing)
                ,("poolDictionaries:",St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral "")) Nothing Nothing)
                ,("category:",St.KeywordArgument (St.PrimaryLiteral (St.StringLiteral cat)) Nothing Nothing)]))) Nothing ->
      Just (FileOutClassDeclaration sp cl (words ivar) (words cvar) cat)
    _ -> Nothing

evalSegmentClassComment :: FileOutSegment -> Maybe FileOutEntry
evalSegmentClassComment fo =
  case fo of
    FileOutEvalSegment txt -> parseFileOutClassComment (St.stParseInitial St.basicExpression txt)
    _ -> Nothing

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

parseMethodsForMethod :: Bool -> St.Identifier -> St.MethodCategory -> String -> St.MethodDefinition
parseMethodsForMethod isClassMethod classname category txt =
  (St.stParse (St.methodDefinition (classname ++ if isClassMethod then " class" else "")) txt)
  {St.methodCategory = Just (St.unquoteQuotedString category)}

readerSegmentMethodDefinitions :: FileOutSegment -> Maybe FileOutEntry
readerSegmentMethodDefinitions fo =
  case fo of
    FileOutReaderSegment r c ->
      case words r of
        classname:"methodsFor:":category:_ ->
          Just (FileOutMethodsFor classname False (map (parseMethodsForMethod False classname category) c))
        classname:"class":"methodsFor:":category:_ ->
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

evalFileOutSegment :: FileOutSegment -> Maybe FileOutEntry
evalFileOutSegment = trySequence [evalSegmentClassDeclaration,evalSegmentClassComment,readerSegmentMethodDefinitions]

evalFileOut :: FileOut -> [Maybe FileOutEntry]
evalFileOut = map evalFileOutSegment

evalFileOutSubset :: FileOut -> [FileOutEntry]
evalFileOutSubset = mapMaybe evalFileOutSegment

evalFileOutOrError :: FileOut -> [FileOutEntry]
evalFileOutOrError = map (fromMaybe (error "evalFileOut")) . evalFileOut

{-

load fn = loadFileOut ("/home/rohan/sw/stsc3/st/" ++ fn)
nms = words "Base Core Env Event Haskell Math Random UGen-Composite UI"
x <- mapM (\nm -> load ("SC3-" ++ nm ++ ".st")) nms
mapM_ (print . evalFileOutOrError) x

-}
