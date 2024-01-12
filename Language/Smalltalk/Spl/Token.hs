-- | Token type for Spl
module Language.Smalltalk.Spl.Token where

data Token
  = ArityQualifiedIdentifier String
  | AssignmentOperator
  | BinarySelector String
  | Colon
  | ColonColon
  | Comma
  | Dot
  | DotDot
  | DoubleQuotedString String
  | Equals
  | FalseIdentifier
  | Float Double
  | Identifier String
  | Integer Integer
  | Keyword String
  | LeftBrace
  | LeftBracket
  | LeftParen
  | Let
  | NilIdentifier
  | RightBrace
  | RightBracket
  | RightParen
  | SelfIdentifier
  | SemiColon
  | SingleQuotedString String
  | TrueIdentifier
  | VerticalBar
  deriving (Eq, Show)
