-- | Printer for abstract syntax tree (Ast) for SuperCollider (Sc).
module Language.Smalltalk.SuperCollider.Ast.Print where

import Data.List {- base -}
import Text.Printf {- base -}

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
    ScPrimaryArrayExpression x -> inBrackets (scBasicExpressionSeqPrint x)
    ScPrimaryDictionaryExpression x -> scBasicExpressionPrint (scDictionaryToBasicExpression x)
    ScPrimaryImplicitMessageSend x a -> x ++ inParen (scBasicExpressionSeqPrint a)

scJoin :: [String] -> String
scJoin = concat

scBlockBodyPrint :: ScBlockBody -> String
scBlockBodyPrint (ScBlockBody arg tmp stm) =
  scJoin
  [maybePrint scBlockArgumentsPrint arg
  ,maybePrint (scJoin . map scTemporariesPrint) tmp
  ,maybePrint scStatementsPrint stm]

scVariablePrint :: ScVariable -> String
scVariablePrint (name, maybeValue) =
  case maybeValue of
    Nothing -> name
    Just value -> unwords [name, "=", scLiteralPrint value]

scBlockArgumentsPrint :: [ScBlockArgument] -> String
scBlockArgumentsPrint x = scJoin ["arg ", intercalate ", " (map scVariablePrint x), "; "]

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

scBasicExpressionSeqPrint :: [ScBasicExpression] -> String
scBasicExpressionSeqPrint = intercalate ", " . map scBasicExpressionPrint

scMessagesPrint :: ScMessages -> String
scMessagesPrint m =
  case m of
    ScMessagesDot m1 m2 -> scJoin
                           [scJoin (map scDotMessagePrint m1)
                           ,maybePrint scBinaryMessagesPrint m2]
    ScMessagesBinary m1 -> scBinaryMessagesPrint m1

scDotMessagePrint :: ScDotMessage -> String
scDotMessagePrint (ScDotMessage i a) = scJoin [".",i,if null a then "" else inParen (scBasicExpressionSeqPrint a)]

inParen :: String -> String
inParen x = "(" ++ x ++ ")"

inBraces :: String -> String
inBraces x = "{ " ++ x ++ " }"

inBrackets :: String -> String
inBrackets x = "[" ++ x ++ "]"

{-
> let x = ScBasicExpression (ScPrimaryIdentifier "x") Nothing
> scKeywordArgumentsPrint [ScKeywordArgument (Just "mul:") x,ScKeywordArgument (Just "add:") x]
scKeywordArgumentsPrint :: [ScKeywordArgument] -> String
scKeywordArgumentsPrint = inParen . intercalate ", " . map scKeywordArgumentPrint
-}

scBinaryMessagePrint :: ScBinaryMessage -> String
scBinaryMessagePrint (ScBinaryMessage i a) = scJoin [" ",i," ",scBinaryArgumentPrint a]

scBinaryMessagesPrint :: [ScBinaryMessage] -> String
scBinaryMessagesPrint = scJoin . map scBinaryMessagePrint

scBinaryArgumentPrint :: ScBinaryArgument -> String
scBinaryArgumentPrint (ScBinaryArgument p m) =
  scJoin [scPrimaryPrint p,maybePrint (scJoin . map scDotMessagePrint) m]

{-
-- > scKeywordArgumentPrint (ScKeywordArgument (Just "mul:") (ScBasicExpression (ScPrimaryIdentifier "x") Nothing))
scKeywordArgumentPrint :: ScKeywordArgument -> String
scKeywordArgumentPrint (ScKeywordArgument k e) =
  scJoin [maybePrint id k,scBasicExpressionPrint e]
-}

scExpressionPrint :: ScExpression -> String
scExpressionPrint e =
  case e of
    ScExprAssignment i e1 -> scJoin [i," = ",scExpressionPrint e1]
    ScExprBasic e1 -> scBasicExpressionPrint e1

{- | Prefix each line of comment with //.

> scCommentPrint "" == ""
-}
scCommentPrint :: ScComment -> String
scCommentPrint = unlines . map ("// " ++) . lines

scInitializerDefinitionPrint :: ScInitializerDefinition -> String
scInitializerDefinitionPrint (ScInitializerDefinition cmt tmp stm) =
  maybe "" scCommentPrint cmt ++ unlines (maybe [] (map scTemporariesPrint) tmp ++ [maybe "" scStatementsPrint stm])

scMethodDefinitionPrint :: ScMethodDefinition -> String
scMethodDefinitionPrint (ScMethodDefinition classSide name body) =
  unwords
  [if classSide then "* " ++ name else name
  ,inBraces (scBlockBodyPrint body)]

scClassDefinitionPrint :: ScClassDefinition -> String
scClassDefinitionPrint (ScClassDefinition nm sc iv cv mt) =
  scJoin
  [scJoin [nm, maybePrint (' ':) sc]
  ," "
  ,inBraces
    (scJoin
     [maybePrint (\l -> printf "classvar %s; " (intercalate ", " (map scVariablePrint l))) cv
     ,maybePrint (\l -> printf "var %s; " (intercalate ", " (map scVariablePrint l))) iv
     ,unwords (map scMethodDefinitionPrint mt)])]

scClassExtensionPrint :: ScClassExtension -> String
scClassExtensionPrint (ScClassExtension nm mt) =
  scJoin
  ["+ " ++ nm
  ," "
  ,inBraces
    (scJoin
     [unwords (map scMethodDefinitionPrint mt)])]
