-- | Printer for abstract syntax tree (Ast) for Stc.
module Language.Smalltalk.Stc.Ast.Print where

import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}
import Language.Smalltalk.Stc.Ast {- stsc3 -}

maybePrint :: (a -> String) -> Maybe a -> String
maybePrint f x = maybe "" f x

stcLiteralPrint :: St.Literal -> String
stcLiteralPrint lit =
  case lit of
    St.NumberLiteral n -> St.number_pp n
    St.StringLiteral s -> printf "\"%s\"" s
    St.CharacterLiteral c -> printf "$%c" c
    St.SymbolLiteral s -> printf "'%s'" s
    St.SelectorLiteral s -> printf "\\%s" (St.selector_pp s)
    St.ArrayLiteral a -> printf "#[%s]" (St.strjnWith ',' (map (either stcLiteralPrint id) a))

stcPrimaryPrint :: StcPrimary -> String
stcPrimaryPrint p =
  case p of
    StcPrimaryIdentifier x -> x
    StcPrimaryLiteral x -> stcLiteralPrint x
    StcPrimaryBlock x -> inBraces (stcBlockBodyPrint x)
    StcPrimaryExpression x -> inParen (stcExpressionPrint x)
    StcPrimaryArrayExpression x -> inBrackets (stcBasicExpressionSeqPrint x)
    StcPrimaryDictionaryExpression x -> stcBasicExpressionPrint (stcDictionaryToBasicExpression x)
    StcPrimaryImplicitMessageSend x a -> x ++ inParen (stcBasicExpressionSeqPrint a)

stcJoin :: [String] -> String
stcJoin = concat

stcBlockBodyPrint :: StcBlockBody -> String
stcBlockBodyPrint (StcBlockBody arg tmp stm) =
  stcJoin
    [ maybePrint stcBlockArgumentsPrint arg
    , maybePrint (stcJoin . map stcTemporariesPrint) tmp
    , maybePrint stcStatementsPrint stm
    ]

stcVariablePrint :: StcVariable -> String
stcVariablePrint (name, maybeValue) =
  case maybeValue of
    Nothing -> name
    Just value -> unwords [name, "=", stcLiteralPrint value]

stcBlockArgumentsPrint :: [StcBlockArgument] -> String
stcBlockArgumentsPrint x = stcJoin ["arg ", intercalate ", " (map stcVariablePrint x), "; "]

stcStatementsPrint :: StcStatements -> String
stcStatementsPrint s =
  case s of
    StcStatementsReturn (StcReturnStatement e) -> "^" ++ stcExpressionPrint e
    StcStatementsExpression e Nothing -> stcExpressionPrint e
    StcStatementsExpression e (Just x) -> stcJoin [stcExpressionPrint e, "; ", stcStatementsPrint x]

stcTemporaryPrint :: StcTemporary -> String
stcTemporaryPrint (x, y) =
  case y of
    Nothing -> x
    Just z -> stcJoin [x, " = ", stcBasicExpressionPrint z]

stcTemporariesPrint :: StcTemporaries -> String
stcTemporariesPrint tmp = stcJoin ["var ", intercalate ", " (map stcTemporaryPrint tmp), "; "]

stcBasicExpressionPrint :: StcBasicExpression -> String
stcBasicExpressionPrint (StcBasicExpression p m) =
  stcJoin [stcPrimaryPrint p, maybePrint stcMessagesPrint m]

stcBasicExpressionSeqPrint :: [StcBasicExpression] -> String
stcBasicExpressionSeqPrint = intercalate ", " . map stcBasicExpressionPrint

stcMessagesPrint :: StcMessages -> String
stcMessagesPrint m =
  case m of
    StcMessagesDot m1 m2 ->
      stcJoin
        [ stcJoin (map stcDotMessagePrint m1)
        , maybePrint stcBinaryMessagesPrint m2
        ]
    StcMessagesBinary m1 -> stcBinaryMessagesPrint m1

stcDotMessagePrint :: StcDotMessage -> String
stcDotMessagePrint (StcDotMessage i a) = stcJoin [".", i, if null a then "" else inParen (stcBasicExpressionSeqPrint a)]

inParen :: String -> String
inParen x = "(" ++ x ++ ")"

inBraces :: String -> String
inBraces x = "{ " ++ x ++ " }"

inBrackets :: String -> String
inBrackets x = "[" ++ x ++ "]"

{-
> let x = StcBasicExpression (StcPrimaryIdentifier "x") Nothing
> stcKeywordArgumentsPrint [StcKeywordArgument (Just "mul:") x,StcKeywordArgument (Just "add:") x]
stcKeywordArgumentsPrint :: [StcKeywordArgument] -> String
stcKeywordArgumentsPrint = inParen . intercalate ", " . map stcKeywordArgumentPrint
-}

stcBinaryMessagePrint :: StcBinaryMessage -> String
stcBinaryMessagePrint (StcBinaryMessage (i, x) a) =
  let b = case x of
        Nothing -> i
        Just x' -> i ++ "." ++ x'
  in stcJoin [" ", b, " ", stcBinaryArgumentPrint a]

stcBinaryMessagesPrint :: [StcBinaryMessage] -> String
stcBinaryMessagesPrint = stcJoin . map stcBinaryMessagePrint

stcBinaryArgumentPrint :: StcBinaryArgument -> String
stcBinaryArgumentPrint (StcBinaryArgument p m) =
  stcJoin [stcPrimaryPrint p, maybePrint (stcJoin . map stcDotMessagePrint) m]

{-
-- > stcKeywordArgumentPrint (StcKeywordArgument (Just "mul:") (StcBasicExpression (StcPrimaryIdentifier "x") Nothing))
stcKeywordArgumentPrint :: StcKeywordArgument -> String
stcKeywordArgumentPrint (StcKeywordArgument k e) =
  stcJoin [maybePrint id k,stcBasicExpressionPrint e]
-}

stcExpressionPrint :: StcExpression -> String
stcExpressionPrint e =
  case e of
    StcExprAssignment i e1 -> stcJoin [i, " = ", stcExpressionPrint e1]
    StcExprBasic e1 -> stcBasicExpressionPrint e1

{- | Prefix each line of comment with //.

> stcLineCommentPrint "" == ""
-}
stcLineCommentPrint :: StcComment -> String
stcLineCommentPrint = unlines . map ("// " ++) . lines

stcBracketCommentPrint :: StcComment -> String
stcBracketCommentPrint cmt = "/*" ++ cmt ++ "*/"

stcInitializerDefinitionPrint :: StcInitializerDefinition -> String
stcInitializerDefinitionPrint (StcInitializerDefinition cmt tmp stm) =
  maybe "" stcLineCommentPrint cmt ++ unlines (maybe [] (map stcTemporariesPrint) tmp ++ [maybe "" stcStatementsPrint stm])

stcMethodDefinitionPrint :: StcMethodDefinition -> String
stcMethodDefinitionPrint (StcMethodDefinition classSide name body _ comment) =
  unwords
    [ maybe "" stcBracketCommentPrint comment
    , if classSide then "* " ++ name else name
    , inBraces (stcBlockBodyPrint body)
    ]

stcClassDefinitionPrint :: StcClassDefinition -> String
stcClassDefinitionPrint (StcClassDefinition nm sc iv cv mt _ cmt) =
  stcJoin
    [ fromMaybe "" cmt
    , stcJoin [nm, maybePrint (" : " ++) sc]
    , " "
    , inBraces
        ( stcJoin
            [ maybePrint (\l -> printf "classvar %s; " (intercalate ", " (map stcVariablePrint l))) cv
            , maybePrint (\l -> printf "var %s; " (intercalate ", " (map stcVariablePrint l))) iv
            , unwords (map stcMethodDefinitionPrint mt)
            ]
        )
    ]

stcClassExtensionPrint :: StcClassExtension -> String
stcClassExtensionPrint (StcClassExtension nm mt) =
  stcJoin
    [ "+ " ++ nm
    , " "
    , inBraces
        ( stcJoin
            [unwords (map stcMethodDefinitionPrint mt)]
        )
    ]
