{- | Translate from the C-Smalltalk(Stc) / SuperCollider (Sc) Ast to the Smalltalk (St) Ast.
     Requires that the Sc Ast has been rewritten to have only constructs that can be translated to St.
     See "Language.Smalltalk.SuperCollider.Rewrite"
-}
module Language.Smalltalk.SuperCollider.Translate where

import Data.Bifunctor {- base -}
import Data.Char {- base -}
import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr as Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr.Print as Expr.Print {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as Ansi.Print {- stsc3 -}

import Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc.Lexer {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc.Parser {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Rewrite as Sc.Rewrite {- stsc3 -}

import qualified Language.Smalltalk.Spl.Lexer as Spl.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Spl.Parser as Spl.Parser {- stsc3 -}

{- | This is for translating, it allows either
     a Unary sequence with an optional ending n-ary message,
     or a single trailing n-ary.
-}
scDotMessagesForSmalltalk :: [ScDotMessage] -> ([ScDotMessage], Maybe ScDotMessage)
scDotMessagesForSmalltalk m =
  if scDotMessagesHaveNary m
    then case break scDotMessageIsNary m of
      (lhs, []) -> (lhs, Nothing)
      (lhs, [k]) -> (lhs, Just k)
      r -> error ("scDotMessagesForSmalltalk: " ++ show (m, r))
    else (m, Nothing)

scFloatConstant :: St.Identifier -> St.Primary
scFloatConstant x =
  St.PrimaryExpression
    ( St.ExprBasic
        ( St.BasicExpression
            (St.PrimaryIdentifier "Float")
            (Just (St.MessagesUnary [St.UnaryMessage x] Nothing Nothing))
            Nothing
        )
    )

-- | The Sc implicit message send x(...) is translated as (x apply: {...})
scImplicitMessageSendSt :: St.Identifier -> [ScBasicExpression] -> St.Primary
scImplicitMessageSendSt x a =
  St.PrimaryExpression
    ( St.ExprBasic
        ( St.BasicExpression
            (St.PrimaryIdentifier x)
            ( Just
                ( St.MessagesKeyword
                    ( St.KeywordMessage
                        [
                          ( "apply:"
                          , St.KeywordArgument (St.PrimaryArrayExpression (map scBasicExpressionSt a)) Nothing Nothing
                          )
                        ]
                    )
                )
            )
            Nothing
        )
    )

scPrimarySt :: ScPrimary -> St.Primary
scPrimarySt p =
  case p of
    ScPrimaryIdentifier x ->
      case x of
        "pi" -> scFloatConstant "pi"
        "inf" -> scFloatConstant "infinity"
        _ -> St.PrimaryIdentifier x
    ScPrimaryLiteral x -> St.PrimaryLiteral x
    ScPrimaryBlock x -> St.PrimaryBlock (scBlockBodySt x)
    ScPrimaryExpression x -> St.PrimaryExpression (scExpressionSt x)
    ScPrimaryArrayExpression x -> St.PrimaryArrayExpression (map scBasicExpressionSt x)
    ScPrimaryDictionaryExpression x -> scPrimarySt (scBasicExpressionToPrimary (scDictionaryToBasicExpression x))
    ScPrimaryImplicitMessageSend x a -> scImplicitMessageSendSt x a

scBlockBodySt :: ScBlockBody -> St.BlockBody
scBlockBodySt (ScBlockBody arg tmp stm) =
  St.BlockBody Nothing (fmap (map fst) arg) (fmap scTemporariesSt tmp) (fmap scStatementsSt stm)

scBinaryArgumentSt :: ScBinaryArgument -> St.BinaryArgument
scBinaryArgumentSt (ScBinaryArgument p m) =
  case m of
    Nothing -> St.BinaryArgument (scPrimarySt p) Nothing
    Just d ->
      case scDotMessagesForSmalltalk d of
        (u, Nothing) -> St.BinaryArgument (scPrimarySt p) (Just (map scDotMessageUnarySt u))
        _ -> error ("scBinaryArgumentSt: " ++ show (ScBinaryArgument p m))

scBinaryMessageSt :: ScBinaryMessage -> St.BinaryMessage
scBinaryMessageSt (ScBinaryMessage (i, x) a) =
  case x of
    Nothing -> St.BinaryMessage i (scBinaryArgumentSt a)
    Just x' -> St.BinaryMessage (i ++ "." ++ x') (scBinaryArgumentSt a)

scArgumentSt :: ScBasicExpression -> St.KeywordArgument
scArgumentSt e =
  St.KeywordArgument
    (St.basicExpressionToPrimary (scBasicExpressionSt e))
    Nothing
    Nothing

-- | The implicit extension keyword, currently "value", perhaps should be "with"
scImplicitKeywordName :: String
scImplicitKeywordName = "value"

{- | For .stc to .st translation "c.at(i, put: j)" means "c at: i put: j".
     In addition "p.q(i, j)" is understood to mean "p q: i value: j".
     That is, if the message name has fewer parts than there are arguments, supply the implicit name "value" for the missing parts.
-}
scDotMessageKeywordSt :: ScDotMessage -> St.KeywordMessage
scDotMessageKeywordSt (ScDotMessage nm arg) =
  let extendWithImplicit = True
      nmParts = Split.splitOn ":" nm
      nmSize = length nmParts
      argSize = length arg
      messageParts =
        if extendWithImplicit && nmSize < argSize
          then take argSize (nmParts ++ repeat scImplicitKeywordName)
          else nmParts
  in if null arg
      then error "scDotMessageKeywordSt: Unary"
      else
        if length messageParts == argSize
          then St.KeywordMessage (zipWith (\i j -> (i ++ ":", scArgumentSt j)) messageParts arg)
          else error "scDotMessageKeywordSt: N-ary messages name degree mismatch"

scDotMessageUnarySt :: ScDotMessage -> St.UnaryMessage
scDotMessageUnarySt (ScDotMessage i a) =
  case a of
    [] -> St.UnaryMessage i
    _ -> error ("scDotMessageUnarySt: argument: " ++ show a)

scMessagesSt :: ScMessages -> St.Messages
scMessagesSt m =
  case m of
    ScMessagesDot m1 m2 ->
      case scDotMessagesForSmalltalk m1 of
        ([], Just k) -> St.MessagesKeyword (scDotMessageKeywordSt k)
        (u, k) ->
          St.MessagesUnary
            (map scDotMessageUnarySt u)
            (fmap (map scBinaryMessageSt) m2)
            (fmap scDotMessageKeywordSt k)
    ScMessagesBinary m1 -> St.MessagesBinary (map scBinaryMessageSt m1) Nothing

scTemporarySt :: ScTemporary -> St.Identifier
scTemporarySt t =
  case t of
    (i, Nothing) -> i
    _ -> error "scTemporarySt"

-- | Temporaries must be in correct form.
scTemporariesSt :: [ScTemporaries] -> St.Temporaries
scTemporariesSt t =
  case t of
    [t1] -> St.Temporaries (map scTemporarySt t1)
    _ -> error "scTemporariesSt"

scReturnStatementSt :: ScReturnStatement -> St.ReturnStatement
scReturnStatementSt (ScReturnStatement e) = St.ReturnStatement (scExpressionSt e)

scStatementsSt :: ScStatements -> St.Statements
scStatementsSt stm =
  case stm of
    ScStatementsReturn x -> St.StatementsReturn (scReturnStatementSt x)
    ScStatementsExpression x y -> St.StatementsExpression (scExpressionSt x) (fmap scStatementsSt y)

scReturnStatement :: ScReturnStatement -> St.ReturnStatement
scReturnStatement (ScReturnStatement x) = St.ReturnStatement (scExpressionSt x)

scBasicExpressionSt :: ScBasicExpression -> St.BasicExpression
scBasicExpressionSt (ScBasicExpression x y) =
  St.BasicExpression (scPrimarySt x) (fmap scMessagesSt y) Nothing

scExpressionSt :: ScExpression -> St.Expression
scExpressionSt e =
  case e of
    ScExprAssignment i e1 -> St.ExprAssignment (St.Assignment i (scExpressionSt e1))
    ScExprBasic e1 -> St.ExprBasic (scBasicExpressionSt e1)

scInitializerDefinitionSt :: ScInitializerDefinition -> St.InitializerDefinition
scInitializerDefinitionSt (ScInitializerDefinition cmt tmp stm) =
  St.InitializerDefinition cmt (fmap scTemporariesSt tmp) (fmap scStatementsSt stm)

-- * C-Smalltalk translator

{- | Separate out any leading // prefixed comment

>>> stcLeadingComment "only\na\nprogram\n"
("","only\na\nprogram\n")

>>> stcLeadingComment "// a\n// comment\nthen\na\nprogram\n"
("a\ncomment\n","then\na\nprogram\n")
-}
stcLeadingComment :: String -> (String, String)
stcLeadingComment = bimap (unlines . map (drop 3)) unlines . span (isPrefixOf "// ") . lines

-- | Stc parse
stcParseToSc :: String -> ScInitializerDefinition
stcParseToSc = Sc.Parser.superColliderParserInitializerDefinition . Sc.Lexer.alexScanTokens

-- | Parse C-Smalltalk InitializerDefinition.
stcParseInitializerDefinition :: String -> St.InitializerDefinition
stcParseInitializerDefinition s =
  let (c, p) = stcLeadingComment s
      eSc = scInitializerDefinitionSetComment c (stcParseToSc p)
  in scInitializerDefinitionSt (Sc.Rewrite.scInitializerDefinitionRewrite eSc)

{- | Translate C-Smalltalk program text to Smalltalk.

>>> stcToSt "(p - q).m"
"(p - q) m .\n"

>>> stcToSt "p(q.r(i).s).t(j) + k"
"((p apply: {(q r: i) s}) t: j) + k .\n"

>>> stcToSt "SinOsc(220,0.5)"
"(SinOsc apply: {220. 0.5}) .\n"

>>> stcToSt "x(i) + y(j,k)"
"(x apply: {i}) + (y apply: {j. k}) .\n"

>>> stcToSt "x(i, y(j, k))"
"(x apply: {i. (y apply: {j. k})}) .\n"

>>> stcToSt "p + q.r.s(a).t.u(b)"
"p + (((q r s: a)) t u: b) .\n"

>>> stcToSt "p + q.r + s.t(u) + v()"
"p + q r + (s t: u) + (v apply: {}) .\n"

>>> stcToSt "f.value(i, j)"
"f value: i value: j .\n"

>>> stcToSt "c.put(i, j)"
"c put: i value: j .\n"

>>> stcToSt "p(q.r(i).s).t + k"
"(p apply: {(q r: i) s}) t + k .\n"

>>> stcToSt "// commentary\nprogram"
"\"commentary\n\" program .\n"

>>> stcToSt "[1, 2, 3, 4, 5]"
"{1. 2. 3. 4. 5} .\n"

>>> stcToSt "p.if { q } { r }"
"p if: [ q .\n ] value: [ r .\n ] .\n"

>>> stcToSt "(x: p, y: q)"
"(Dictionary newFromPairs: {x. p. y. q}) .\n"

>>> stcToSt "p.q * x.f { y }"
"p q * (x f: [ y .\n ]) .\n"

>>> stcToSt "p +++ q"
"p +++ q .\n"

>>> stcToSt "f(x) { y }"
"(f apply: {x. [ y .\n ]}) .\n"

>>> stcToSt "p. /* q */ r"
"p r .\n"

>>> stcToSt "f(p: x, q: y)"
"(f p: x q: y) .\n"

>>> stcToSt "c.at(i, put: x)"
"c at: i put: x .\n"

> stcToSt "f.m(p: x, q: y)" -- error
-}
stcToSt :: String -> String
stcToSt = Ansi.Print.initializerDefinition_pp . stcParseInitializerDefinition

-- | Parse .stc and translate to Init Expr.
stcToExpr :: String -> Expr.Expr
stcToExpr =
  Expr.initializerDefinitionExpr
    . stcParseInitializerDefinition

-- | exprPrintJs of stcToExpr
stcToJs :: Maybe String -> String -> String
stcToJs maybePrefix =
  Expr.Print.exprPrintJs (Expr.Print.jsRenamerFromTable maybePrefix Expr.Print.jsDefaultRenamingTable)
    . stcToExpr

-- | exprPrintScheme of stcToExpr
stcToScheme :: String -> String
stcToScheme =
  Expr.Print.exprPrintScheme (Expr.Print.jsRenamerFromTable Nothing Expr.Print.jsDefaultRenamingTable)
    . stcToExpr

stcToAst :: String -> String
stcToAst = show . stcToExpr

-- | Statements list of .stc Init Expr.
stcToExprStm :: String -> [Expr.Expr]
stcToExprStm = Expr.initStatements . stcToExpr

{- | Parse and print .stc

>>> stcToExprToStc "(a - b).c"
"(a - b).c"
-}
stcToExprToStc :: String -> String
stcToExprToStc = Expr.Print.exprPrintStc True . stcToExpr

{- | Print .stc as .sc.  Apply is not implicit in .sc.

>>> stcToSc "SinOsc(440, 0)"
"SinOsc.apply([440, 0])"

>>> stcToSc "f(x, y)"
"f.apply([x, y])"
-}
stcToSc :: String -> String
stcToSc = Expr.Print.exprPrintStc False . stcToExpr

-- * Spl

{- | Separate out any leading single line block comment.

>>> splLeadingComment "only\na\nprogram\n"
("","only\na\nprogram\n")

>>> splLeadingComment "(* a comment *)\nthen\na\nprogram\n"
("a comment","then\na\nprogram\n")
-}
splLeadingComment :: String -> (String, String)
splLeadingComment x =
  let isComment str = "(* " `isPrefixOf` str && " *)" `isSuffixOf` str
      removeComment str = List.dropRight 3 (drop 3 str)
  in case lines x of
      (l1 : l') -> if isComment l1 then (removeComment l1, unlines l') else ("", x)
      _ -> ("", x)

{- | Separate out any leading prefix comment

>>> splLeadingPrefixComment ";;" ";; a\n;; comment\nthen\na\nprogram\n"
("a\ncomment\n","then\na\nprogram\n")
-}
splLeadingPrefixComment :: String -> String -> (String, String)
splLeadingPrefixComment prefix =
  bimap (unlines . map (drop 3)) unlines
    . span (isPrefixOf (prefix ++ " "))
    . lines

{- | In Spl f(x, y) and x.f(y) are interchangeable.
In particular f may be a "constructor" procedure.
Stc implements methods for the required cases, following the usual St case rule.
To translate Spl, all dot expressions where the selector is capitalised are rewritten.

>>> splRewriteDotExpressions "m.MidiCps"
"m.midiCps"
-}
splRewriteDotExpressions :: String -> String
splRewriteDotExpressions txt =
  case txt of
    [] -> []
    '.' : c : txt' -> if isUpper c then '.' : toLower c : splRewriteDotExpressions txt' else '.' : c : splRewriteDotExpressions txt'
    c : txt' -> c : splRewriteDotExpressions txt'

{- | Spl allows binary operator characters not allowed by St(/c).
These are re-written to allowed operators.

>>> splRewriteBinaryOperators "{ } !^ 6"
"{ } %~ 6"

>>> splRewriteBinaryOperators "LfPar:/2"
"LfPar"
-}
splRewriteBinaryOperators :: String -> String
splRewriteBinaryOperators txt =
  case txt of
    [] -> []
    ' ' : '<' : '!' : ' ' : txt' -> ' ' : '<' : '|' : ' ' : splRewriteBinaryOperators txt'
    ' ' : '!' : ' ' : txt' -> ' ' : '%' : '%' : ' ' : splRewriteBinaryOperators txt'
    c : txt' -> c : splRewriteBinaryOperators txt'

-- | Spl parse to Sc
splParseToSc :: String -> ScInitializerDefinition
splParseToSc = Spl.Parser.splParser . Spl.Lexer.alexScanTokens

-- | Parse Spl to St.
splParseToSt :: String -> St.InitializerDefinition
splParseToSt s =
  let (c, p) = splLeadingComment s
      p' = splRewriteBinaryOperators (splRewriteDotExpressions p)
      eSc = scInitializerDefinitionSetComment c (splParseToSc p')
  in scInitializerDefinitionSt (Sc.Rewrite.scInitializerDefinitionRewrite eSc)

-- | Parse .spl and translate to Init Expr.
splToExpr :: String -> Expr.Expr
splToExpr =
  Expr.initializerDefinitionExpr
    . splParseToSt

{- | Spl to St

>>> splToSt "{ Rand(0, 1) } !^ 4"
"[ (Rand apply: {0. 1}) .\n ] %~ 4 .\n"

>>> splToSt "[1 2 3 4 5]"
"{1. 2. 3. 4. 5} .\n"

>>> splToSt "[1.2 3.4 5]"
"{1.2. 3.4. 5} .\n"

>>> splToSt "[p q r s t]"
"{p. q. r. s. t} .\n"

>>> splToSt "{ :x | x * x }"
"[ :x | x * x .\n ] .\n"

>>> splToSt "let x = 1; x"
"| x |\n x := 1 .\n x .\n"

>>> splToSt "x.F"
"x f .\n"

>>> splToSt "let x = 5; x := x + 1; x"
"| x |\n x := 5 .\n x := x + 1 .\n x .\n"

>>> splToSt "if(p) { q } { r }"
"(if apply: {p. [ q .\n ]. [ r .\n ]}) .\n"

>>> splToSt "[p ++ q, x ++ [y, z], a ++ [b c]]"
"{p ++ q. x ++ {y. z}. a ++ {b. c}} .\n"

>>> splToSt "[1 2 3; 4 5 6]"
"{{1. 2. 3}. {4. 5. 6}} .\n"
-}
splToSt :: String -> String
splToSt = Ansi.Print.initializerDefinition_pp . splParseToSt

-- * Method and Class definitions

{- | Generate primaryFactoryMethod method for named class.

> stPrimaryFactoryMethod ("SinOsc class", True) "freq:phase:"
-}
stPrimaryFactoryMethod :: (St.Identifier, Bool) -> String -> St.MethodDefinition
stPrimaryFactoryMethod cl nm =
  let pat = St.UnaryPattern "primaryFactoryMethod"
      xpr = St.ExprBasic (St.BasicExpression (St.PrimaryLiteral (St.SymbolLiteral nm)) Nothing Nothing)
      stm = St.StatementsReturn (St.ReturnStatement xpr)
  in St.MethodDefinition cl Nothing pat Nothing (Just stm) Nothing Nothing Nothing

{- | Translate Sc method to St.
Rewrites temporaries and for precedence.
The class method "constructor" is special.
It's pattern is derived from it's arguments names, and it generates a primary factory method.
-}
scMethodDefinitionToSt :: St.Identifier -> ScMethodDefinition -> [St.MethodDefinition]
scMethodDefinitionToSt cl_nm md =
  let is_cl = isClassMethod md
      st_cl_nm = if is_cl then cl_nm ++ " class" else cl_nm
      blk = Sc.Rewrite.scBlockBodyRewrite (methodBody md)
      blk_args = maybe [] (map fst) (blockArguments blk)
      md_nm = methodName md
      is_constructor = is_cl && md_nm == "constructor"
      pat =
        if is_constructor
          then case blk_args of
            [] -> St.UnaryPattern "new"
            args -> St.KeywordPattern (zip (map (++ ":") args) args)
          else case blk_args of
            [] -> St.UnaryPattern md_nm
            [arg] ->
              if St.isBinaryIdentifier md_nm
                then St.BinaryPattern md_nm arg
                else St.KeywordPattern [(md_nm, arg)]
            args -> St.KeywordPattern (zip (St.keywordSelectorElements md_nm) args)
      tmp = fmap scTemporariesSt (blockTemporaries blk)
      stm = fmap scStatementsSt (blockStatements blk)
      st_md = St.MethodDefinition (st_cl_nm, is_cl) Nothing pat tmp stm Nothing Nothing Nothing
  in if is_constructor
      then [st_md, stPrimaryFactoryMethod (st_cl_nm, is_cl) (if null blk_args then "new" else concatMap (++ ":") blk_args)]
      else [st_md]

-- | Translate Sc class to St.
scClassDefinitionToSt :: ScClassDefinition -> St.ClassDefinition
scClassDefinitionToSt cd =
  let nm = className cd
      (cm, im) = scClassDefinitionPartitionMethods cd
  in St.ClassDefinition
      nm
      (superclassName cd)
      St.noInstanceState
      (maybe [] (map fst) (classInstanceVariableNames cd))
      (maybe [] (map fst) (classVariableNames cd))
      []
      (concatMap (scMethodDefinitionToSt nm) im)
      (concatMap (scMethodDefinitionToSt nm) cm)
      Nothing
      (classCategory cd)
      (classComment cd)

{-

rw = scToSt
scToSt "p.q(r: i)" == "p q: {#'r:' -> i} .\n"
scToSt "p.q:r(i)" -- error ; this is an error in Sc and is reported here but with an odd message
scToSt "p.q()" -- error ; this was dis-allowed for stcToSt ; it's not neccesary in Sc but it is allowed

-}
