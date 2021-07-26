-- | Printer for abstract syntax tree (Ast) for SuperCollider (Sc).
module Language.Smalltalk.SuperCollider.Ast.Print where

import Data.List {- maybe -}
import Text.Printf {- maybe -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}

maybePrint :: (a -> String) -> Maybe a -> String
maybePrint f x = maybe "" f x

scLiteralPrint :: St.Literal -> String
scLiteralPrint lit =
  case lit of
    St.NumberLiteral n -> St.number_pp n
    St.StringLiteral s -> printf "\"%s\"" s
    St.CharacterLiteral c -> printf "$%c" c
    St.SymbolLiteral s -> printf "'%s'" s
    St.SelectorLiteral s -> printf "\\%s" (St.selector_pp s)
    St.ArrayLiteral a -> printf "#[%s]" (St.strjnWith ',' (map (either scLiteralPrint id) a))

scPrimaryPrint :: ScPrimary -> String
scPrimaryPrint p =
  case p of
    ScPrimaryIdentifier x -> x
    ScPrimaryLiteral x -> scLiteralPrint x
    ScPrimaryBlock x -> inBraces (scBlockBodyPrint x)
    ScPrimaryExpression x -> inParen (scExpressionPrint x)
    ScPrimaryArrayExpression x -> inBrackets (intercalate ", " (map scBasicExpressionPrint x))

scJoin :: [String] -> String
scJoin = concat

scBlockBodyPrint :: ScBlockBody -> String
scBlockBodyPrint (ScBlockBody a t s) =
  scJoin
  [maybePrint scBlockArgumentsPrint a
  ,maybePrint (scJoin . map scTemporariesPrint) t
  ,maybePrint scStatementsPrint s]

scBlockArgumentsPrint :: [St.BlockArgument] -> String
scBlockArgumentsPrint = intercalate ", "

scStatementsPrint :: ScStatements -> String
scStatementsPrint s =
  case s of
    ScStatementsReturn (ScReturnStatement e) -> "^" ++ scExpressionPrint e
    ScStatementsExpression e Nothing -> scExpressionPrint e
    ScStatementsExpression e (Just x) -> scJoin [scExpressionPrint e,"; ",scStatementsPrint x]

scTemporaryPrint :: ScTemporary -> String
scTemporaryPrint (x,y) =
  case y of
    Nothing -> x
    Just z -> scJoin [x," = ",scBasicExpressionPrint z]

scTemporariesPrint :: ScTemporaries -> String
scTemporariesPrint tmp = scJoin ["var ",intercalate ", " (map scTemporaryPrint tmp),"; "]

scBasicExpressionPrint :: ScBasicExpression -> String
scBasicExpressionPrint (ScBasicExpression p m) =
  scJoin [scPrimaryPrint p,maybePrint scMessagesPrint m]

scMessagesPrint :: ScMessages -> String
scMessagesPrint m =
  case m of
    ScMessagesDot m1 m2 -> scJoin
                           [scJoin (map scDotMessagePrint m1)
                           ,maybePrint scBinaryMessagesPrint m2]
    ScMessagesBinary m1 -> scBinaryMessagesPrint m1

scDotMessagePrint :: ScDotMessage -> String
scDotMessagePrint (ScDotMessage i a) = scJoin [".",i,maybePrint scKeywordArgumentsPrint a]

inParen :: String -> String
inParen x = "(" ++ x ++ ")"

inBraces :: String -> String
inBraces x = "{" ++ x ++ "}"

inBrackets :: String -> String
inBrackets x = "[" ++ x ++ "]"

scKeywordArgumentsPrint :: [ScKeywordArgument] -> String
scKeywordArgumentsPrint = inParen . intercalate ", " . map scKeywordArgumentPrint

scBinaryMessagePrint :: ScBinaryMessage -> String
scBinaryMessagePrint (ScBinaryMessage i a) = scJoin [" ",i," ",scBinaryArgumentPrint a]

scBinaryMessagesPrint :: [ScBinaryMessage] -> String
scBinaryMessagesPrint = scJoin . map scBinaryMessagePrint

scBinaryArgumentPrint :: ScBinaryArgument -> String
scBinaryArgumentPrint (ScBinaryArgument p m) =
  scJoin [scPrimaryPrint p,maybePrint (scJoin . map scDotMessagePrint) m]

scKeywordArgumentPrint :: ScKeywordArgument -> String
scKeywordArgumentPrint (ScKeywordArgument k e) = scJoin
                                                 [maybePrint (" " ++) k
                                                 ,scBasicExpressionPrint e]

scExpressionPrint :: ScExpression -> String
scExpressionPrint e =
  case e of
    ScExprAssignment i e1 -> scJoin [i," = ",scExpressionPrint e1]
    ScExprBasic e1 -> scBasicExpressionPrint e1
