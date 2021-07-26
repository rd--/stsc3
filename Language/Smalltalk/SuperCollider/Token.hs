-- | Token type for SuperCollider (modified from Language.Smalltalk.Ansi.Token)
module Language.Smalltalk.SuperCollider.Token where

data Token =
    LeftBracket
  | RightBracket
  | Dot
  | Comma
  | SemiColon
  | LeftBrace
  | RightBrace
  | LeftParen
  | RightParen
  | HashLeftBracket

  | NilIdentifier
  | TrueIdentifier
  | FalseIdentifier
  | ThisIdentifier

  | Identifier String
  | Keyword String
  | BinarySelector String
  | ReturnOperator
  | AssignmentOperator
  | Float Double
  | Integer Integer
  | QuotedChar Char
  | QuotedString String
  | HashedString String

  | Arg
  | Var

  deriving (Eq, Show)
