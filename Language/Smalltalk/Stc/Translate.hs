{- | Translate from the C-Smalltalk(Stc) Ast to the Smalltalk (St) Ast.
     Requires that the Stc Ast has been rewritten to have only constructs that can be translated to St.
     See "Language.Smalltalk.Stc.Rewrite"
-}
module Language.Smalltalk.Stc.Translate where

import Data.Bifunctor {- base -}
import Data.Char {- base -}
import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr as Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr.Print as Expr.Print {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as Ansi.Print {- stsc3 -}

import Language.Smalltalk.Stc.Ast {- stsc3 -}
import qualified Language.Smalltalk.Stc.Lexer as Stc.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Stc.Parser as Stc.Parser {- stsc3 -}
import qualified Language.Smalltalk.Stc.Rewrite as Stc.Rewrite {- stsc3 -}

import qualified Language.Smalltalk.Spl.Lexer as Spl.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Spl.Parser as Spl.Parser {- stsc3 -}

import qualified Language.Smalltalk.SuperCollider.Parser as SuperCollider.Parser {- stsc3 -}

{- | This is for translating, it allows either
     a Unary sequence with an optional ending n-ary message,
     or a single trailing n-ary.
-}
stcDotMessagesForSmalltalk :: [StcDotMessage] -> ([StcDotMessage], Maybe StcDotMessage)
stcDotMessagesForSmalltalk m =
  if stcDotMessagesHaveNary m
    then case break stcDotMessageIsNary m of
      (lhs, []) -> (lhs, Nothing)
      (lhs, [k]) -> (lhs, Just k)
      r -> error ("stcDotMessagesForSmalltalk: " ++ show (m, r))
    else (m, Nothing)

stcFloatConstant :: St.Identifier -> St.Primary
stcFloatConstant x =
  St.PrimaryExpression
    ( St.ExprBasic
        ( St.BasicExpression
            (St.PrimaryIdentifier "Float")
            (Just (St.MessagesUnary [St.UnaryMessage x] Nothing Nothing))
            Nothing
        )
    )

-- | The Stc implicit message send x(...) is translated as (x apply: {...})
stcImplicitMessageSendSt :: St.Identifier -> [StcBasicExpression] -> St.Primary
stcImplicitMessageSendSt x a =
  St.PrimaryExpression
    ( St.ExprBasic
        ( St.BasicExpression
            (St.PrimaryIdentifier x)
            ( Just
                ( St.MessagesKeyword
                    ( St.KeywordMessage
                        [
                          ( "apply:"
                          , St.KeywordArgument (St.PrimaryArrayExpression (map stcBasicExpressionSt a)) Nothing Nothing
                          )
                        ]
                    )
                )
            )
            Nothing
        )
    )

stcPrimarySt :: StcPrimary -> St.Primary
stcPrimarySt p =
  case p of
    StcPrimaryIdentifier x ->
      case x of
        "pi" -> stcFloatConstant "pi"
        "inf" -> stcFloatConstant "infinity"
        _ -> St.PrimaryIdentifier x
    StcPrimaryLiteral x -> St.PrimaryLiteral x
    StcPrimaryBlock x -> St.PrimaryBlock (stcBlockBodySt x)
    StcPrimaryExpression x -> St.PrimaryExpression (stcExpressionSt x)
    StcPrimaryArrayExpression x -> St.PrimaryArrayExpression (map stcBasicExpressionSt x)
    StcPrimaryDictionaryExpression x -> stcPrimarySt (stcBasicExpressionToPrimary (stcDictionaryToBasicExpression x))
    StcPrimaryImplicitMessageSend x a -> stcImplicitMessageSendSt x a

stcBlockBodySt :: StcBlockBody -> St.BlockBody
stcBlockBodySt (StcBlockBody arg tmp stm) =
  St.BlockBody Nothing (fmap (map fst) arg) (fmap stcTemporariesSt tmp) (fmap stcStatementsSt stm)

stcBinaryArgumentSt :: StcBinaryArgument -> St.BinaryArgument
stcBinaryArgumentSt (StcBinaryArgument p m) =
  case m of
    Nothing -> St.BinaryArgument (stcPrimarySt p) Nothing
    Just d ->
      case stcDotMessagesForSmalltalk d of
        (u, Nothing) -> St.BinaryArgument (stcPrimarySt p) (Just (map stcDotMessageUnarySt u))
        _ -> error ("stcBinaryArgumentSt: " ++ show (StcBinaryArgument p m))

stcBinaryMessageSt :: StcBinaryMessage -> St.BinaryMessage
stcBinaryMessageSt (StcBinaryMessage (i, x) a) =
  case x of
    Nothing -> St.BinaryMessage i (stcBinaryArgumentSt a)
    Just x' -> St.BinaryMessage (i ++ "." ++ x') (stcBinaryArgumentSt a)

stcArgumentSt :: StcBasicExpression -> St.KeywordArgument
stcArgumentSt e =
  St.KeywordArgument
    (St.basicExpressionToPrimary (stcBasicExpressionSt e))
    Nothing
    Nothing

-- | The implicit extension keyword, currently "value", perhaps should be "with"
stcImplicitKeywordName :: String
stcImplicitKeywordName = "value"

{- | For .stc to .st translation "c.at(i, put: j)" means "c at: i put: j".
     In addition "p.q(i, j)" is understood to mean "p q: i value: j".
     That is, if the message name has fewer parts than there are arguments, supply the implicit name "value" for the missing parts.
-}
stcDotMessageKeywordSt :: StcDotMessage -> St.KeywordMessage
stcDotMessageKeywordSt (StcDotMessage nm arg) =
  let extendWithImplicit = True
      nmParts = Split.splitOn ":" nm
      nmSize = length nmParts
      argSize = length arg
      messageParts =
        if extendWithImplicit && nmSize < argSize
          then take argSize (nmParts ++ repeat stcImplicitKeywordName)
          else nmParts
  in if null arg
      then error "stcDotMessageKeywordSt: Unary"
      else
        if length messageParts == argSize
          then St.KeywordMessage (zipWith (\i j -> (i ++ ":", stcArgumentSt j)) messageParts arg)
          else error "stcDotMessageKeywordSt: N-ary messages name degree mismatch"

stcDotMessageUnarySt :: StcDotMessage -> St.UnaryMessage
stcDotMessageUnarySt (StcDotMessage i a) =
  case a of
    [] -> St.UnaryMessage i
    _ -> error ("stcDotMessageUnarySt: argument: " ++ show a)

stcMessagesSt :: StcMessages -> St.Messages
stcMessagesSt m =
  case m of
    StcMessagesDot m1 m2 ->
      case stcDotMessagesForSmalltalk m1 of
        ([], Just k) -> St.MessagesKeyword (stcDotMessageKeywordSt k)
        (u, k) ->
          St.MessagesUnary
            (map stcDotMessageUnarySt u)
            (fmap (map stcBinaryMessageSt) m2)
            (fmap stcDotMessageKeywordSt k)
    StcMessagesBinary m1 -> St.MessagesBinary (map stcBinaryMessageSt m1) Nothing

stcTemporarySt :: StcTemporary -> St.Identifier
stcTemporarySt t =
  case t of
    (i, Nothing) -> i
    _ -> error "stcTemporarySt"

-- | Temporaries must be in correct form.
stcTemporariesSt :: [StcTemporaries] -> St.Temporaries
stcTemporariesSt t =
  case t of
    [t1] -> St.Temporaries (map stcTemporarySt t1)
    _ -> error "stcTemporariesSt"

stcReturnStatementSt :: StcReturnStatement -> St.ReturnStatement
stcReturnStatementSt (StcReturnStatement e) = St.ReturnStatement (stcExpressionSt e)

stcStatementsSt :: StcStatements -> St.Statements
stcStatementsSt stm =
  case stm of
    StcStatementsReturn x -> St.StatementsReturn (stcReturnStatementSt x)
    StcStatementsExpression x y -> St.StatementsExpression (stcExpressionSt x) (fmap stcStatementsSt y)

stcReturnStatement :: StcReturnStatement -> St.ReturnStatement
stcReturnStatement (StcReturnStatement x) = St.ReturnStatement (stcExpressionSt x)

stcBasicExpressionSt :: StcBasicExpression -> St.BasicExpression
stcBasicExpressionSt (StcBasicExpression x y) =
  St.BasicExpression (stcPrimarySt x) (fmap stcMessagesSt y) Nothing

stcExpressionSt :: StcExpression -> St.Expression
stcExpressionSt e =
  case e of
    StcExprAssignment i e1 -> St.ExprAssignment (St.Assignment i (stcExpressionSt e1))
    StcExprBasic e1 -> St.ExprBasic (stcBasicExpressionSt e1)

stcInitializerDefinitionSt :: StcInitializerDefinition -> St.InitializerDefinition
stcInitializerDefinitionSt (StcInitializerDefinition cmt tmp stm) =
  St.InitializerDefinition cmt (fmap stcTemporariesSt tmp) (fmap stcStatementsSt stm)

-- * C-Smalltalk (.stc) translator

{- | Separate out any leading // prefixed comment

>>> stcLeadingComment "only\na\nprogram\n"
("","only\na\nprogram\n")

>>> stcLeadingComment "// a\n// comment\nthen\na\nprogram\n"
("a\ncomment\n","then\na\nprogram\n")
-}
stcLeadingComment :: String -> (String, String)
stcLeadingComment = bimap (unlines . map (drop 3)) unlines . span (isPrefixOf "// ") . lines

-- | Stc parse
stcParseToStc :: String -> StcInitializerDefinition
stcParseToStc = Stc.Parser.parseInitializerDefinition . Stc.Lexer.alexScanTokens

-- | Sc parse
scParseToStc :: String -> StcInitializerDefinition
scParseToStc = SuperCollider.Parser.parseInitializerDefinition . Stc.Lexer.alexScanTokens

parseToSt :: (String -> StcInitializerDefinition) -> String -> St.InitializerDefinition
parseToSt f s =
  let (c, p) = stcLeadingComment s
      eStc = stcInitializerDefinitionSetComment c (f p)
  in stcInitializerDefinitionSt (Stc.Rewrite.stcInitializerDefinitionRewrite eStc)

-- | Parse C-Smalltalk InitializerDefinition.
stcParseToSt :: String -> St.InitializerDefinition
stcParseToSt = parseToSt stcParseToStc

-- | Parse SuperCollider InitializerDefinition.
scParseToSt :: String -> St.InitializerDefinition
scParseToSt = parseToSt scParseToStc

{- | Translate C-Smalltalk program text to Smalltalk.

>>> stcToSt "p := q"
"p := q .\n"

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
stcToSt = Ansi.Print.initializerDefinition_pp . stcParseToSt

scToSt :: String -> String
scToSt = Ansi.Print.initializerDefinition_pp . scParseToSt

{- | Parse .stc and translate to Expr.Expr.

>>> stcToExpr "60.MidiCps"
Init (Just "") [] [Send (Literal (NumberLiteral (Int 60))) (Message (UnarySelector "MidiCps") [])]
-}
stcToExpr :: String -> Expr.Expr
stcToExpr = Expr.initializerDefinitionExpr . stcParseToSt

scToExpr :: String -> Expr.Expr
scToExpr = Expr.initializerDefinitionExpr . scParseToSt

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

{- | Print .stc as .stc.  Apply is not printed implicitly.

>>> stcToStc "SinOsc(440, 0)"
"SinOsc.apply([440, 0])"

>>> stcToStc "f(x, y)"
"f.apply([x, y])"
-}
stcToStc :: String -> String
stcToStc = Expr.Print.exprPrintStc False . stcToExpr

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

{- | Spl parse to Stc -}
splParseToStc :: String -> StcInitializerDefinition
splParseToStc = Spl.Parser.parseInitializerDefinition . Spl.Lexer.alexScanTokens

-- | Parse Spl to St.
splParseToSt :: String -> St.InitializerDefinition
splParseToSt s =
  let (c, p) = splLeadingComment s
      p' = splRewriteBinaryOperators (splRewriteDotExpressions p)
      eStc = stcInitializerDefinitionSetComment c (splParseToStc p')
  in stcInitializerDefinitionSt (Stc.Rewrite.stcInitializerDefinitionRewrite eStc)

-- | Parse .spl and translate to Init Expr.
splToExpr :: String -> Expr.Expr
splToExpr =
  Expr.initializerDefinitionExpr
    . splParseToSt

{- | Spl to St

>>> splToSt "{ Rand(0, 1) } !^ 4" -- Hat operator
"[ (Rand apply: {0. 1}) .\n ] %~ 4 .\n"

>>> splToSt "[1 2 3 4 5]" -- Vector syntax
"{1. 2. 3. 4. 5} .\n"

>>> splToSt "[1.2 3.4 5]"
"{1.2. 3.4. 5} .\n"

>>> splToSt "[p q r s t]"
"{p. q. r. s. t} .\n"

>>> splToSt "{ :x | x * x }" -- Block syntax
"[ :x | x * x .\n ] .\n"

>>> splToSt "let x = 1; x" -- Let syntax
"| x |\n x := 1 .\n x .\n"

>>> splToSt "x.F"
"x f .\n"

>>> splToSt "let x = 5; x := x + 1; x" -- Assignment syntax
"| x |\n x := 5 .\n x := x + 1 .\n x .\n"

>>> splToSt "if(p) { q } { r }"
"(if apply: {p. [ q .\n ]. [ r .\n ]}) .\n"

>>> splToSt "[p ++ q, x ++ [y, z], a ++ [b c]]"
"{p ++ q. x ++ {y. z}. a ++ {b. c}} .\n"

>>> splToSt "[1 2 3; 4 5 6]" -- Matrix syntax
"{{1. 2. 3}. {4. 5. 6}} .\n"

>>> splToSt "f(+)" -- Operator identifier
"(f apply: {+}) .\n"

>>> splToSt "()" -- Empty dictionary
"(Dictionary newFromPairs: {}) .\n"

>>> splToSt "(k: v)" -- Dictionary
"(Dictionary newFromPairs: {k. v}) .\n"

>>> splToSt "i + c::k + j" -- Quoted at syntax
"i + (c at: '''k''') + j .\n"

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

{- | Translate Stc method to St.
Rewrites temporaries and for precedence.
The class method "constructor" is special.
It's pattern is derived from it's arguments names, and it generates a primary factory method.
-}
stcMethodDefinitionToSt :: St.Identifier -> StcMethodDefinition -> [St.MethodDefinition]
stcMethodDefinitionToSt cl_nm md =
  let is_cl = isClassMethod md
      st_cl_nm = if is_cl then cl_nm ++ " class" else cl_nm
      blk = Stc.Rewrite.stcBlockBodyRewrite (methodBody md)
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
      tmp = fmap stcTemporariesSt (blockTemporaries blk)
      stm = fmap stcStatementsSt (blockStatements blk)
      st_md = St.MethodDefinition (st_cl_nm, is_cl) Nothing pat tmp stm Nothing Nothing Nothing
  in if is_constructor
      then [st_md, stPrimaryFactoryMethod (st_cl_nm, is_cl) (if null blk_args then "new" else concatMap (++ ":") blk_args)]
      else [st_md]

-- | Translate Stc class to St.
stcClassDefinitionToSt :: StcClassDefinition -> St.ClassDefinition
stcClassDefinitionToSt cd =
  let nm = className cd
      (cm, im) = stcClassDefinitionPartitionMethods cd
  in St.ClassDefinition
      nm
      (superclassName cd)
      St.noInstanceState
      (maybe [] (map fst) (classInstanceVariableNames cd))
      (maybe [] (map fst) (classVariableNames cd))
      []
      (concatMap (stcMethodDefinitionToSt nm) im)
      (concatMap (stcMethodDefinitionToSt nm) cm)
      Nothing
      (classCategory cd)
      (classComment cd)

{-

rw = stcToSt
stcToSt "p.q(r: i)" == "p q: {#'r:' -> i} .\n"
stcToSt "p.q:r(i)" -- error ; this is an error in Stc and is reported here but with an odd message
stcToSt "p.q()" -- error ; this is dis-allowed

-}
