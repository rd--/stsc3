-- | Printer for abstract syntax tree (Ast) for Spl.
module Language.Smalltalk.Spl.Ast.Print where

import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}
import Language.Smalltalk.Spl.Ast {- stsc3 -}

maybePrint :: (a -> String) -> Maybe a -> String
maybePrint f x = maybe "" f x

splLiteralPrint :: St.Literal -> String
splLiteralPrint lit =
  case lit of
    St.NumberLiteral n -> St.number_pp n
    St.StringLiteral s -> printf "\"%s\"" s
    St.CharacterLiteral c -> printf "$%c" c
    St.SymbolLiteral s -> printf "'%s'" s
    St.SelectorLiteral s -> printf "\\%s" (St.selector_pp s)
    St.ArrayLiteral a -> printf "#[%s]" (St.strjnWith ',' (map (either splLiteralPrint id) a))

splPrimaryPrint :: SplPrimary -> String
splPrimaryPrint p =
  case p of
    SplPrimaryIdentifier x -> x
    SplPrimaryLiteral x -> splLiteralPrint x
    SplPrimaryBlock x -> inBraces (splBlockBodyPrint x)
    SplPrimaryExpression x -> inParen (splExpressionPrint x)
    SplPrimaryArrayExpression x -> inBrackets (splBasicExpressionSeqPrint x)
    SplPrimaryDictionaryExpression x -> splBasicExpressionPrint (splDictionaryToBasicExpression x)
    SplPrimaryImplicitMessageSend x a -> x ++ inParen (splBasicExpressionSeqPrint a)

splJoin :: [String] -> String
splJoin = concat

splBlockBodyPrint :: SplBlockBody -> String
splBlockBodyPrint (SplBlockBody arg tmp stm) =
  splJoin
    [ maybePrint splBlockArgumentsPrint arg
    , maybePrint (splJoin . map splTemporariesPrint) tmp
    , maybePrint splStatementsPrint stm
    ]

splVariablePrint :: SplVariable -> String
splVariablePrint (name, maybeValue) =
  case maybeValue of
    Nothing -> name
    Just value -> unwords [name, "=", splLiteralPrint value]

splBlockArgumentsPrint :: [SplBlockArgument] -> String
splBlockArgumentsPrint x = splJoin ["arg ", intercalate ", " (map splVariablePrint x), "; "]

splStatementsPrint :: SplStatements -> String
splStatementsPrint s =
  case s of
    SplStatementsReturn (SplReturnStatement e) -> "^" ++ splExpressionPrint e
    SplStatementsExpression e Nothing -> splExpressionPrint e
    SplStatementsExpression e (Just x) -> splJoin [splExpressionPrint e, "; ", splStatementsPrint x]

splTemporaryPrint :: SplTemporary -> String
splTemporaryPrint (x, y) =
  case y of
    Nothing -> x
    Just z -> splJoin [x, " = ", splBasicExpressionPrint z]

splTemporariesPrint :: SplTemporaries -> String
splTemporariesPrint tmp = splJoin ["var ", intercalate ", " (map splTemporaryPrint tmp), "; "]

splBasicExpressionPrint :: SplBasicExpression -> String
splBasicExpressionPrint (SplBasicExpression p m) =
  splJoin [splPrimaryPrint p, maybePrint splMessagesPrint m]

splBasicExpressionSeqPrint :: [SplBasicExpression] -> String
splBasicExpressionSeqPrint = intercalate ", " . map splBasicExpressionPrint

splMessagesPrint :: SplMessages -> String
splMessagesPrint m =
  case m of
    SplMessagesDot m1 m2 ->
      splJoin
        [ splJoin (map splDotMessagePrint m1)
        , maybePrint splBinaryMessagesPrint m2
        ]
    SplMessagesBinary m1 -> splBinaryMessagesPrint m1

splDotMessagePrint :: SplDotMessage -> String
splDotMessagePrint (SplDotMessage i a) = splJoin [".", i, if null a then "" else inParen (splBasicExpressionSeqPrint a)]

inParen :: String -> String
inParen x = "(" ++ x ++ ")"

inBraces :: String -> String
inBraces x = "{ " ++ x ++ " }"

inBrackets :: String -> String
inBrackets x = "[" ++ x ++ "]"

{-
> let x = SplBasicExpression (SplPrimaryIdentifier "x") Nothing
> splKeywordArgumentsPrint [SplKeywordArgument (Just "mul:") x,SplKeywordArgument (Just "add:") x]
splKeywordArgumentsPrint :: [SplKeywordArgument] -> String
splKeywordArgumentsPrint = inParen . intercalate ", " . map splKeywordArgumentPrint
-}

splBinaryMessagePrint :: SplBinaryMessage -> String
splBinaryMessagePrint (SplBinaryMessage (i, x) a) =
  let b = case x of
        Nothing -> i
        Just x' -> i ++ "." ++ x'
  in splJoin [" ", b, " ", splBinaryArgumentPrint a]

splBinaryMessagesPrint :: [SplBinaryMessage] -> String
splBinaryMessagesPrint = splJoin . map splBinaryMessagePrint

splBinaryArgumentPrint :: SplBinaryArgument -> String
splBinaryArgumentPrint (SplBinaryArgument p m) =
  splJoin [splPrimaryPrint p, maybePrint (splJoin . map splDotMessagePrint) m]

{-
-- > splKeywordArgumentPrint (SplKeywordArgument (Just "mul:") (SplBasicExpression (SplPrimaryIdentifier "x") Nothing))
splKeywordArgumentPrint :: SplKeywordArgument -> String
splKeywordArgumentPrint (SplKeywordArgument k e) =
  splJoin [maybePrint id k,splBasicExpressionPrint e]
-}

splExpressionPrint :: SplExpression -> String
splExpressionPrint e =
  case e of
    SplExprAssignment i e1 -> splJoin [i, " = ", splExpressionPrint e1]
    SplExprBasic e1 -> splBasicExpressionPrint e1

{- | Prefix each line of comment with //.

> splLineCommentPrint "" == ""
-}
splLineCommentPrint :: SplComment -> String
splLineCommentPrint = unlines . map ("// " ++) . lines

splBracketCommentPrint :: SplComment -> String
splBracketCommentPrint cmt = "/*" ++ cmt ++ "*/"

splInitializerDefinitionPrint :: SplInitializerDefinition -> String
splInitializerDefinitionPrint (SplInitializerDefinition cmt tmp stm) =
  maybe "" splLineCommentPrint cmt ++ unlines (maybe [] (map splTemporariesPrint) tmp ++ [maybe "" splStatementsPrint stm])

splMethodDefinitionPrint :: SplMethodDefinition -> String
splMethodDefinitionPrint (SplMethodDefinition classSide name body _ comment) =
  unwords
    [ maybe "" splBracketCommentPrint comment
    , if classSide then "* " ++ name else name
    , inBraces (splBlockBodyPrint body)
    ]

splClassDefinitionPrint :: SplClassDefinition -> String
splClassDefinitionPrint (SplClassDefinition nm sc iv cv mt _ cmt) =
  splJoin
    [ fromMaybe "" cmt
    , splJoin [nm, maybePrint (" : " ++) sc]
    , " "
    , inBraces
        ( splJoin
            [ maybePrint (\l -> printf "classvar %s; " (intercalate ", " (map splVariablePrint l))) cv
            , maybePrint (\l -> printf "var %s; " (intercalate ", " (map splVariablePrint l))) iv
            , unwords (map splMethodDefinitionPrint mt)
            ]
        )
    ]

splClassExtensionPrint :: SplClassExtension -> String
splClassExtensionPrint (SplClassExtension nm mt) =
  splJoin
    [ "+ " ++ nm
    , " "
    , inBraces
        ( splJoin
            [unwords (map splMethodDefinitionPrint mt)]
        )
    ]
