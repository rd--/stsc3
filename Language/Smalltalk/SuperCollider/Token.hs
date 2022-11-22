-- | Token type for SuperCollider (modified from Language.Smalltalk.Ansi.Token)
module Language.Smalltalk.SuperCollider.Token where

data Token =
    LeftBracket
  | RightBracket
  | Dot
  | Comma
  | SemiColon
  | Colon
  | LeftBrace
  | RightBrace
  | LeftParen
  | RightParen
  | HashLeftBracket

  | NilIdentifier
  | TrueIdentifier
  | FalseIdentifier
  | SelfIdentifier

  | Identifier String
  | Keyword String
  | BinarySelector String
  | KeywordSelector String
  | ReturnOperator
  | EqualsOperator
  | AssignmentOperator
  | ClassExtensionOperator
  | ClassMethodOperator
  | Float Double
  | Integer Integer
  | QuotedChar Char
  | QuotedString String
  | HashedString String
  | Comment String

  | Arg
  | Var
  | ClassVar

  deriving (Eq, Show)
