-- | Token type for SuperCollider (modified from Language.Smalltalk.Ansi.Token)
module Language.Smalltalk.SuperCollider.Token where

data Token =
    VerticalBar -- 3.4.2 |
  | LeftBracket -- [ -- 3.4.4
  | RightBracket -- ]
  | Dot -- . -- 3.4.5
  | Comma
  | SemiColon -- ; -- 3.4.5.3
  | LeftBrace -- {
  | RightBrace -- }
  | LeftParen -- (
  | RightParen -- )

  | NilIdentifier -- 3.4.7
  | TrueIdentifier -- 3.4.7
  | FalseIdentifier -- 3.4.7
  | ThisIdentifier -- 3.4.7

  | Identifier String -- 3.5.3
  | Keyword String -- 3.5.4
  | BinarySelector String -- 3.5.5
  | ReturnOperator -- 3.5.5
  | AssignmentOperator -- 3.5.5
  | Float Double -- 3.5.6
  | Integer Integer -- 3.5.6
  | QuotedChar Char -- 3.5.8
  | QuotedString String -- 3.5.8
  | HashedString String -- 3.5.9

  | Arg
  | Var

  deriving (Eq, Show)
