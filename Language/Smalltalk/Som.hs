-- | Parser and pretty printer for SOM in terms of Smalltalk.
module Language.Smalltalk.Som where

import qualified Text.Parsec as P {- parsec -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

equalSign :: St.P Char
equalSign = St.lexeme (P.char '=')

openParen :: St.P Char
openParen = St.lexeme (P.char '(')

closeParen :: St.P Char
closeParen = St.lexeme (P.char ')')

data MethodBlock =
  MethodBlock (Maybe St.Temporaries) (Maybe St.Statements)
  deriving (Eq, Show)

methodBlock :: St.P MethodBlock
methodBlock = do
  _ <- openParen
  t <- P.optionMaybe St.temporaries
  s <- P.optionMaybe St.statements
  _ <- closeParen
  return (MethodBlock t s)

primitive :: St.P MethodBlock
primitive = do
  p <- St.lexeme (P.string "primitive")
  let be = St.BasicExpression (St.PrimaryIdentifier p) Nothing Nothing
      se = St.StatementsExpression (St.ExprBasic be) Nothing
  return (MethodBlock Nothing (Just se))

{- | The SOM separator is an allowed Smalltalk operator name.
     It can therefore form the start of a Smalltalk method definition.
     This parser must disallow this, this is done using notFollowedBy as a prefix rule.

> St.stParse methodDefinition "---- new = (| t | q. r. ^s)" -- fail
-}
methodDefinition :: St.P St.MethodDefinition
methodDefinition = do
  P.notFollowedBy separator
  p <- St.messagePattern
  _ <- equalSign
  MethodBlock t s <- primitive P.<|> methodBlock
  return (St.MethodDefinition p t s)

-- > St.stParse separator "-----------" == "----"
separator :: St.P String
separator = St.lexeme (P.string "----" St.>>~ P.many (P.char '-'))

classSide :: St.P ([St.Identifier], [St.MethodDefinition])
classSide = do
  _ <- separator
  t <- P.option [] St.temporariesIdentifierSequence
  m <- P.many methodDefinition
  return (t,m)

-- > St.stParse classDefinition "\"Set class definition\" Set = Object (|items|)"
-- > St.stParse classDefinition "\"Object class definition\" Object = nil ()"
classDefinition :: St.P St.ClassDefinition
classDefinition = do
  P.optional St.separator
  className <- St.identifier
  _ <- equalSign
  superclassName <- P.optionMaybe St.identifier
  _ <- openParen
  instanceVariableNames <- P.option [] St.temporariesIdentifierSequence
  instanceMethods <- P.many methodDefinition
  (classVariableNames,classMethods) <- P.option ([],[]) classSide
  _ <- closeParen
  return (St.ClassDefinition
          className
          superclassName
          St.noInstanceState
          instanceVariableNames
          classVariableNames
          []
          instanceMethods
          classMethods
          Nothing)
