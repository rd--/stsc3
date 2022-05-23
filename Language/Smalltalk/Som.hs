{- | Parser for SOM in terms of the Ansi Smalltalk Ast.

     Som has a Class per file syntax.
     The grammar is: <https://github.com/SOM-st/SOM/blob/master/specification/SOM.g4>
     The parser here follows the sequence in the specification.
-}
module Language.Smalltalk.Som where

import System.FilePath {- filepath -}

import qualified Text.Parsec as P {- parsec -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Annotate as Annotate {- stsc3 -}

-- * Parser

{- | String parsing in SOM has alternate rules, see also somEscapedString.
     The ANSI parser could be modified to have a selectable rule for this.

> rewriteSomQuotingToSmalltalk "\\'" == "''" -- quoted '
> rewriteSomQuotingToSmalltalk "'\\''" == "''''" -- quoted ' in string
> rewriteSomQuotingToSmalltalk "'\\\\\''" == "'\\\\\''" -- quoted \ and quoted ' in string
> rewriteSomQuotingToSmalltalk "Lexing = ( f = ( ^['\\'', '\\\\'] ) )"
> rewriteSomQuotingToSmalltalk "''" == "''" -- empty string
-}
rewriteSomQuotingToSmalltalk :: String -> String
rewriteSomQuotingToSmalltalk =
  let f s =
        case s of
          [] -> []
          '\\' : '\\' : s' -> '\\' : '\\' : f s'
          '\\' : '\'' : s' -> '\'' : '\'' : f s'
          c : s' -> c : f s'
  in f

{- | Run parser for Som class definition.

> parseSomClassDefinition "\"Set class definition\" Set = Object (|items|)"
> parseSomClassDefinition "\"Object class definition\" Object = nil ()"
> parseSomClassDefinition "Class = ()"
> parseSomClassDefinition "Lexing = ( f = ( ^['\\'', '\\\\'] ) )"
-}
parseSomClassDefinition :: String -> St.ClassDefinition
parseSomClassDefinition = St.stParse classDefinition . rewriteSomQuotingToSmalltalk

{- | Class definition.
     In Som if the superclassName is not specified it is "Object".
     In Som the end of the class chain is indicated by "nil".
     For Ansi "nil" becomes Nothing and empty becomes "Object".
     Note: fails if the first method definition is for '|' and there are no variables.

> St.stParse classDefinition "Boolean = ( | aBoolean = ( ^self or: aBoolean ) )"
-}
classDefinition :: St.P St.ClassDefinition
classDefinition = do
  P.optional St.separator
  className <- St.identifier
  _ <- equalSign
  somSuperclassName <- P.optionMaybe St.identifier
  let superclassName =
        case somSuperclassName of
          Just "nil" -> Nothing
          Nothing -> Just "Object"
          _ -> somSuperclassName
  (instanceVariableNames,instanceMethods,classVariableNames,classMethods) <- St.inParentheses (classBody className)
  return (St.ClassDefinition
          className
          superclassName
          St.noInstanceState
          instanceVariableNames
          classVariableNames
          []
          (map Annotate.methodDefinitionAnnotateBlocks instanceMethods)
          (map Annotate.methodDefinitionAnnotateBlocks classMethods)
          Nothing
          Nothing
          Nothing)

-- | Instance fields and methods, optionally class fields and methods.
classBody :: St.Identifier -> St.P ([St.Identifier], [St.MethodDefinition], [St.Identifier], [St.MethodDefinition])
classBody cl = do
  iv <- P.option [] St.temporariesIdentifierSequence
  im <- P.many (methodDefinition cl)
  (cv,cm) <- P.option ([],[]) (classSide (St.metaclassName cl))
  return (iv,im,cv,cm)

-- | Class fields and class methods.
classSide :: St.Identifier -> St.P ([St.Identifier], [St.MethodDefinition])
classSide cl = do
  _ <- separator
  t <- P.option [] St.temporariesIdentifierSequence
  m <- P.many (methodDefinition cl)
  return (t,m)

-- | Method block.  Arguments are given by the methodPattern.  Primitives have a label.
data MethodBlock =
  MethodBlock (Maybe St.Temporaries) (Maybe St.Statements) (Maybe St.Primitive)
  deriving (Eq, Show)

{- | Method definition.
The St Ast method node has a literal field for primitives, which is here set to 0.

> St.stParse (methodDefinition "Number") "sumSqr: x = ( ^(self * self) + (x * x) )"
> St.stParse (methodDefinition "Boolean") "| aBoolean = ( ^self or: aBoolean )"
> St.stParse (methodDefinition "Boolean") "& aBoolean = ( ^self and: aBoolean )"
-}
methodDefinition :: St.Identifier -> St.P St.MethodDefinition
methodDefinition cl = do
  P.notFollowedBy separator
  pat <- St.messagePattern
  _ <- equalSign
  MethodBlock tmp stm prm <- somPrimitive P.<|> St.inParentheses methodBlock
  return (St.MethodDefinition cl Nothing pat tmp stm prm Nothing Nothing)

methodBlock :: St.P MethodBlock
methodBlock = do
  prm <- P.optionMaybe St.primitive
  tmp <- P.optionMaybe St.temporaries
  stm <- P.optionMaybe St.statements
  return (MethodBlock tmp stm prm)

-- * Lexer

-- | Som allows primitive as a singular reserved word indicating that a method is Primitive.
somPrimitive :: St.P MethodBlock
somPrimitive = do
  _ <- St.lexeme (P.string "primitive")
  return (MethodBlock Nothing Nothing (Just (St.primitiveOf 0)))

{- | Seperator for instance and class methods.
     The SOM separator is an allowed Smalltalk operator name.
     It can therefore form the start of a Smalltalk method definition.
     This parser must disallow this, which is done using notFollowedBy as a prefix rule.

> St.stParse separator "-----------" == "----"
> St.stParse (methodDefinition "") "---- new = (| t | q. r. ^s)" -- fail
-}
separator :: St.P String
separator = St.lexeme (P.string "----" St.>>~ P.many (P.char '-'))

-- | Not a token in St.
equalSign :: St.P Char
equalSign = St.lexeme (P.char '=')

-- * Strings

{- | Escape characters (beyond '') are not specified in the Ansi document.
     This parser scans two character escape codes into the single character they represent.
     Som requires the following:

     \t   a tab character
     \b   a backspace character
     \n   a newline character
     \r   a carriage return character
     \f   a formfeed character
     \'   a single quote character
     \\   backslash character
     \0   zero byte character

     The Som rule for \' is not compatible with the Smalltalk rules.
     In Smalltalk '''' is a string with a single quote character.
     In Som it is two empty strings alongside one another.
     In Som '\'' is a string with a single quote character.
     In Smalltalk it is an unterminated string.

> p = St.stParse escapedChar
> p "\\n" == '\n'
> p "\\0" == '\0'
> p "\\'" == '\''
> p "\\\\" == '\\'
-}
escapedChar :: St.P Char
escapedChar = do
  d <- P.char '\\'
  c <- P.oneOf "tbnrf'\\0"
  return (read ['\'',d,c,'\''])

{- | Any character that is not one of the characters that are escaped at escapedChar.
     Som also requires that newlines be allowed in strings, so these are excluded.
     The implementation here uses an Ansi Smalltalk parser, so the input may include unquoted quotes.
     For this reason they are allowed.

> p = St.stParse nonEscapedChar
> p "x" == 'x'
> p "\n" == '\n'
> p "'" == '\''
-}
nonEscapedChar :: St.P Char
nonEscapedChar = P.noneOf "\t\b\r\f\\\0" -- '

-- | Either an escaped or a non-escaped character.
stringCharacter :: St.P Char
stringCharacter = nonEscapedChar P.<|> escapedChar

{- | Parse an escaped string body.

> p = St.stParse escapedStringBody
> p "\\n" == "\n"
> p "x\\'" == "x'"
> p "'" == "'"
-}
escapedStringBody :: St.P String
escapedStringBody = P.many stringCharacter

{- | In Som this should be run on the string literals that are derived by the Smalltalk parser.

> somEscapedString "\\t" == "\t"
> somEscapedString "\'" == "'" -- single quote character in Som
> somEscapedString "''" == "''" -- single quote character in Smalltalk
> somEscapedString "\\\\"
-}
somEscapedString :: String -> String
somEscapedString = St.stParse escapedStringBody

-- * Class list

-- | The list of standard (required) Som classes.
somStandardClassList :: [St.Identifier]
somStandardClassList =
  ["Array"
  ,"Block1","Block2","Block3","Block"
  ,"Boolean"
  ,"Class"
  ,"Dictionary"
  ,"Double"
  ,"False"
  ,"HashEntry"
  ,"Hashtable"
  ,"Integer"
  ,"Metaclass"
  ,"Method"
  ,"Nil"
  ,"Object"
  ,"Pair"
  ,"Primitive"
  ,"Set"
  ,"String"
  ,"Symbol"
  ,"System"
  ,"True"
  ,"Vector"]

-- * IO

-- | Load ClassDefinition from named file.
somLoadClassDefinition :: FilePath -> IO St.ClassDefinition
somLoadClassDefinition fn = do
  txt <- readFile fn
  return (parseSomClassDefinition txt)

-- | Load list of class definitions into association list.
somLoadClassList :: FilePath -> [St.Identifier] -> IO [St.ClassDefinition]
somLoadClassList somDirectory classList = do
  let somClassFilename nm = somDirectory </> nm <.> "som"
  mapM (somLoadClassDefinition . somClassFilename) classList
