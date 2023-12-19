-- | Token type for ANSI Smalltalk
module Language.Smalltalk.Ansi.Token where

data Token
  = VerticalBar -- 3.4.2 |
  | LeftBracket -- [ -- 3.4.4
  | RightBracket -- ]
  | Dot -- . -- 3.4.5
  | SemiColon -- ; -- 3.4.5.3
  | LeftBrace -- {
  | RightBrace -- }
  | HashLeftParen -- #( 3.4.6.6
  | Hash -- #
  | Colon -- :
  | LeftParen -- (
  | RightParen -- )
  | LeftAngleBracket -- <
  | RightAngleBracket -- >
  | NilIdentifier -- 3.4.7
  | TrueIdentifier -- 3.4.7
  | FalseIdentifier -- 3.4.7
  | SelfIdentifier -- 3.4.7
  | SuperIdentifier -- 3.4.7
  | PrimitiveIdentifier -- primitive
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
  deriving (-- | UnarySelector String -- 3.5.10
            -- | QuotedSelector String -- 3.5.10 - #Selector
            -- | KeywordSelector String -- 3.5.10 - Keyword+
            Eq, Show)
