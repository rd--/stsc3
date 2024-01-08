-- | Token type for Spl
module Language.Smalltalk.Spl.Token where

data Token
  = LeftBracket
  | RightBracket
  | Dot
  | DotDot
  | Comma
  | SemiColon
  | Colon
  | ColonColon
  | VerticalBar
  | LeftBrace
  | RightBrace
  | LeftParen
  | RightParen
  | NilIdentifier
  | TrueIdentifier
  | FalseIdentifier
  | SelfIdentifier
  | Identifier String
  | ArityQualifiedIdentifier String
  | Keyword String
  | BinarySelector String
  | EqualsOperator
  | AssignmentOperator
  | Float Double
  | Integer Integer
  | DoubleQuotedString String
  | SingleQuotedString String
  | Comment String
  | Let
  deriving (Eq, Show)
