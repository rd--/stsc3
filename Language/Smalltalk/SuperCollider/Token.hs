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
  | NaryMessageName String -- .stc
  | Keyword String
  | BinarySelector String
  | ClassMethodName String
  | ClassExtensionName String
  | ReturnOperator
  | AssignmentOperator
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
