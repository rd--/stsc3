-- | Token type for Stc (modified from Language.Smalltalk.Ansi.Token)
module Language.Smalltalk.Stc.Token where

data Token
  = Arg
  | AssignmentOperator
  | Asterisk
  | BinarySelector String
  | ClassVar
  | Colon
  | ColonColon
  | Comma
  | Dot
  | DotDot
  | DoubleQuotedString String
  | Equals
  | FalseIdentifier
  | Float Double
  | HashLeftBracket
  | Identifier String
  | Integer Integer
  | Keyword String
  | KeywordSelector String
  | LeftBrace
  | LeftBracket
  | LeftParen
  | NilIdentifier
  | Plus
  | QuotedChar Char
  | ReturnOperator
  | RightBrace
  | RightBracket
  | RightParen
  | SelfIdentifier
  | SemiColon
  | SingleQuotedString String
  | TrueIdentifier
  | Var
  | VerticalBar
  deriving (Eq, Show)
