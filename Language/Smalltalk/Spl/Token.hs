-- | Token type for Spl (modified from Language.Smalltalk.Ansi.Token)
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
--  | KeywordSelector String
  | ReturnOperator
  | EqualsOperator
  | AssignmentOperator
  | Float Double
  | Integer Integer
  | DoubleQuotedString String
  | SingleQuotedString String
  | Comment String
  | Arg
  | Var
  | Let
  | ClassVar
  deriving (Eq, Show)
