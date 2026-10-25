-- | Token type for Spl
module Language.Smalltalk.Spl.Token where

data Token
  = ArityQualifiedLowercaseIdentifier String
  | ArityQualifiedUppercaseIdentifier String
  | BinarySelector String
  | Colon
  | ColonColon
  | ColonEquals
  | Comma
  | Dot
  | DotDot
  | DoubleQuotedString String
  | EOF
  | Equals
  | FalseIdentifier
  | Float Double
  | InfinityIdentifier
  | Integer Integer
  | Keyword String
  | LeftBrace
  | LeftBracket
  | LeftParen
  | Let
  | LowercaseIdentifier String
  | NaNIdentifier
  | NilIdentifier
  | RightBrace
  | RightBracket
  | RightParen
  | SemiColon
  | SingleQuotedString String
  | TrueIdentifier
  | UppercaseIdentifier String
  | VerticalBar
  deriving (Eq, Show)
