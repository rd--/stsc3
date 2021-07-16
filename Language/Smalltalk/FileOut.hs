{- | FileOut Code Format

<http://wiki.squeak.org/squeak/1105>

A fileout consists of a sequence of code segments called "chunks" separated by ! character.

Any ! character occurring within the code segment must be doubled to form a chunk.

An empty chunk consisting of one or more whitespace characters terminates the sequence.

These sequence takes one of two forms:

  <chunk>!<chunk>! ... !<whitespace>!
  !<reader>!<stream>!

The parser here accepts only a subset of the second form, that required to add methods to classes.

-}
module Language.Smalltalk.FileOut where

import qualified Text.Parsec as P {- parsec -}

import Language.Smalltalk.Parser as St

-- | A chunk is a text string.
type Chunk = String

-- | A reader is a text string.
type Reader = String

-- | Whitespace is a text string containing only space characters.
type Whitespace = String

-- | Chunks are delimited by !.
--   It should be possible to escape these as !! but that is not implemented.
chunkDelimiter :: P Char
chunkDelimiter = P.char '!' -- non St.lexeme

-- | Any char that is not !.
nonChunkDelimiter :: P Char
nonChunkDelimiter = P.noneOf ['!']

-- | A chunk that has only whitespace.
--
-- > St.stParse emptyChunk " !" == " "
-- > St.stParse emptyChunk " x !" -- error
emptyChunk :: P Whitespace
emptyChunk = P.many P.space St.>>~ chunkDelimiter

-- | An ordinary chunk.
--
-- > St.stParse nonEmptyChunk " x !" == " x "
-- > St.stParse nonEmptyChunk " !" -- error
-- > St.stParse (P.many1 nonEmptyChunk) " x ! y !" == [" x "," y "]
nonEmptyChunk :: P Chunk
nonEmptyChunk =
  (P.try emptyChunk >> P.unexpected "emptyChunk") P.<|>
  (P.many nonChunkDelimiter St.>>~ chunkDelimiter)

-- | A seqence of one or more Chunks.
chunkSequence :: P [Chunk]
chunkSequence = P.many1 (P.try nonEmptyChunk) St.>>~ emptyChunk

-- | A Reader.
reader :: P Reader
reader = nonEmptyChunk

-- | A FileOut segment is either an Eval segment or a Reader segment.
data FileOutSegment
  = FileOutEvalSegment [Chunk]
  | FileOutReaderSegment Reader [Chunk]
  deriving (Eq, Show)

-- | A FileOut is a sequence of segments.
type FileOut = [FileOutSegment]

-- | Parser for Eval segment.
--
-- > St.stParse fileOutEval " x ! y ! !"
fileOutEval :: P FileOutSegment
fileOutEval = fmap FileOutEvalSegment chunkSequence

-- | Parser for Reader segment.
fileOutReader :: P FileOutSegment
fileOutReader = do
  _ <- P.optional (P.many P.space)
  _ <- chunkDelimiter
  r <- reader
  s <- chunkSequence
  return (FileOutReaderSegment r s)

-- | Parser for FileOut segment.
fileOutSegment :: P FileOutSegment
fileOutSegment = fileOutReader P.<|> fileOutEval

{- | Parser for FileOut.

> St.stParse fileOut "'A comment chunk'! !" == [FileOutEvalSegment ["'A comment chunk'"]]
> St.stParse fileOut "x! y! !"
> St.stParse fileOut "!Number methodsFor: 'arithmetic'! midicps ^440 * (2 raisedTo: ((self - 69) * (1 / 12)))! !"
-}
fileOut :: P FileOut
fileOut = P.many1 (P.try fileOutSegment) St.>>~ P.optional (P.spaces)

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