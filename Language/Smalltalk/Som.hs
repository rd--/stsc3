{- | Parser for SOM in terms of the Ansi Smalltalk Ast.

     Som has a Class per file syntax.
     The grammar is: <https://github.com/SOM-st/SOM/blob/master/specification/SOM.g4>
     The parser here follows the sequence in the specification.
-}
module Language.Smalltalk.Som where

import qualified Text.Parsec as P {- parsec -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Annotate as St {- stsc3 -}

{- | Run parser for Som class definition.

> parseSomClassDefinition "\"Set class definition\" Set = Object (|items|)"
> parseSomClassDefinition "\"Object class definition\" Object = nil ()"
> parseSomClassDefinition "\"Object class definition\" Class = ()"
-}
parseSomClassDefinition :: String -> St.ClassDefinition
parseSomClassDefinition = St.stParse classDefinition

{- | Class definition.
     In Som if the superclassName is not specified it is "Object".
     In Som the end of the class chain is indicated by "nil".
     For Ansi "nil" becomes Nothing and empty becomes "Object".
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
          (map St.methodDefinitionAnnotateBlocks instanceMethods)
          (map St.methodDefinitionAnnotateBlocks classMethods)
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

{- | Method definition.

> St.stParse methodDefinition "sumSqr: x = ( ^(self * self) + (x * x) )"
-}
methodDefinition :: St.Identifier -> St.P St.MethodDefinition
methodDefinition cl = do
  P.notFollowedBy separator
  p <- St.messagePattern
  _ <- equalSign
  MethodBlock t s <- primitive P.<|> St.inParentheses methodBlock
  return (St.MethodDefinition cl Nothing p t s)

-- | Method block.  Arguments are given by the methodPattern.
data MethodBlock =
  MethodBlock (Maybe St.Temporaries) (Maybe St.Statements)
  deriving (Eq, Show)

methodBlock :: St.P MethodBlock
methodBlock = do
  t <- P.optionMaybe St.temporaries
  s <- P.optionMaybe St.statements
  return (MethodBlock t s)

-- * Lexer

{- | Primitive is a reserved word indicating that a method is Primitive.
     There is no St Ast node for this
     It is represented as a Method of only the identifier "primitive".
-}
primitive :: St.P MethodBlock
primitive = do
  p <- St.lexeme (P.string "primitive")
  let be = St.BasicExpression (St.PrimaryIdentifier p) Nothing Nothing
      se = St.StatementsExpression (St.ExprBasic be) Nothing
  return (MethodBlock Nothing (Just se))

-- | Predicate to examine a MethodDefinition and decide if it is a Som primitive.
somMethodIsPrimitive :: St.MethodDefinition -> Bool
somMethodIsPrimitive m =
  case m of
    St.MethodDefinition
      _
      _
      _
      Nothing
      (Just (St.StatementsExpression
              (St.ExprBasic
                (St.BasicExpression
                  (St.PrimaryIdentifier "primitive")
                  Nothing
                  Nothing))
              Nothing)) -> True
    _ -> False

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

> p = St.stParse escapedChar
> p "\\n" == '\n'
> p "\\0" == '\0'
-}
escapedChar :: St.P Char
escapedChar = do
  d <- P.char '\\'
  c <- P.oneOf "tbnrf'\\0"
  return (read ['\'',d,c,'\''])

{- | Any character that is not one of the characters that are escaped at escapedChar.
     Som also requires that newlines be allowed in strings, so these are excluded.

> p = St.stParse nonEscapedChar
> p "x" == 'x'
> p "\n" == "\n"
-}
nonEscapedChar :: St.P Char
nonEscapedChar = P.noneOf "\t\b\r\f'\\\0"

-- | Either an escaped or a non-escaped character.
stringCharacter :: St.P Char
stringCharacter = nonEscapedChar P.<|> escapedChar

{- | Parse an escaped string body.

> p = St.stParse escapedStringBody
> p "\\n" == "\n"
> p "x\\'" == "x'"
-}
escapedStringBody :: St.P String
escapedStringBody = P.many stringCharacter

-- | In Som this should be run on the string literals that are derived by the Smalltalk parser.
somEscapedString :: String -> String
somEscapedString = St.stParse escapedStringBody
