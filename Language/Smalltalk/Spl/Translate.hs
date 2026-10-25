{- | Translate from the C-Smalltalk(Spl) Ast to the Smalltalk (St) Ast.
     Requires that the Spl Ast has been rewritten to have only constructs that can be translated to St.
     See "Language.Smalltalk.Spl.Rewrite"
-}
module Language.Smalltalk.Spl.Translate where

import Data.Bifunctor {- base -}
import Data.Char {- base -}
import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr as Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr.Print as Expr.Print {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as Ansi.Print {- stsc3 -}

import Language.Smalltalk.Spl.Ast {- stsc3 -}
import qualified Language.Smalltalk.Spl.Lexer as Spl.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Spl.Parser as Spl.Parser {- stsc3 -}
import qualified Language.Smalltalk.Spl.Rewrite as Spl.Rewrite {- stsc3 -}

import qualified Language.Smalltalk.Spl.Lexer as Spl.Lexer {- stsc3 -}
import qualified Language.Smalltalk.Spl.Parser as Spl.Parser {- stsc3 -}

--import qualified Language.Smalltalk.SuperCollider.Parser as SuperCollider.Parser {- stsc3 -}

{- | This is for translating, it allows either
     a Unary sequence with an optional ending n-ary message,
     or a single trailing n-ary.
-}
splDotMessagesForSmalltalk :: [SplDotMessage] -> ([SplDotMessage], Maybe SplDotMessage)
splDotMessagesForSmalltalk m =
  if splDotMessagesHaveNary m
    then case break splDotMessageIsNary m of
      (lhs, []) -> (lhs, Nothing)
      (lhs, [k]) -> (lhs, Just k)
      r -> error ("splDotMessagesForSmalltalk: " ++ show (m, r))
    else (m, Nothing)

splFloatConstant :: St.Identifier -> St.Primary
splFloatConstant x =
  St.PrimaryExpression
    ( St.ExprBasic
        ( St.BasicExpression
            (St.PrimaryIdentifier "Float")
            (Just (St.MessagesUnary [St.UnaryMessage x] Nothing Nothing))
            Nothing
        )
    )

-- | The Spl implicit message send x(...) is translated as (x apply: {...})
splImplicitMessageSendSt :: St.Identifier -> [SplBasicExpression] -> St.Primary
splImplicitMessageSendSt x a =
  St.PrimaryExpression
    ( St.ExprBasic
        ( St.BasicExpression
            (St.PrimaryIdentifier x)
            ( Just
                ( St.MessagesKeyword
                    ( St.KeywordMessage
                        [
                          ( "apply:"
                          , St.KeywordArgument (St.PrimaryArrayExpression (map splBasicExpressionSt a)) Nothing Nothing
                          )
                        ]
                    )
                )
            )
            Nothing
        )
    )

splPrimarySt :: SplPrimary -> St.Primary
splPrimarySt p =
  case p of
    SplPrimaryIdentifier x ->
      case x of
        "pi" -> splFloatConstant "pi"
        "inf" -> splFloatConstant "infinity"
        _ -> St.PrimaryIdentifier x
    SplPrimaryLiteral x -> St.PrimaryLiteral x
    SplPrimaryBlock x -> St.PrimaryBlock (splBlockBodySt x)
    SplPrimaryExpression x -> St.PrimaryExpression (splExpressionSt x)
    SplPrimaryArrayExpression x -> St.PrimaryArrayExpression (map splBasicExpressionSt x)
    SplPrimaryDictionaryExpression x -> splPrimarySt (splBasicExpressionToPrimary (splDictionaryToBasicExpression x))
    SplPrimaryImplicitMessageSend x a -> splImplicitMessageSendSt x a

splBlockBodySt :: SplBlockBody -> St.BlockBody
splBlockBodySt (SplBlockBody arg tmp stm) =
  St.BlockBody Nothing (fmap (map fst) arg) (fmap splTemporariesSt tmp) (fmap splStatementsSt stm)

splBinaryArgumentSt :: SplBinaryArgument -> St.BinaryArgument
splBinaryArgumentSt (SplBinaryArgument p m) =
  case m of
    Nothing -> St.BinaryArgument (splPrimarySt p) Nothing
    Just d ->
      case splDotMessagesForSmalltalk d of
        (u, Nothing) -> St.BinaryArgument (splPrimarySt p) (Just (map splDotMessageUnarySt u))
        _ -> error ("splBinaryArgumentSt: " ++ show (SplBinaryArgument p m))

splBinaryMessageSt :: SplBinaryMessage -> St.BinaryMessage
splBinaryMessageSt (SplBinaryMessage (i, x) a) =
  case x of
    Nothing -> St.BinaryMessage i (splBinaryArgumentSt a)
    Just x' -> St.BinaryMessage (i ++ "." ++ x') (splBinaryArgumentSt a)

splArgumentSt :: SplBasicExpression -> St.KeywordArgument
splArgumentSt e =
  St.KeywordArgument
    (St.basicExpressionToPrimary (splBasicExpressionSt e))
    Nothing
    Nothing

-- | The implicit extension keyword, currently "value", perhaps should be "with"
splImplicitKeywordName :: String
splImplicitKeywordName = "value"

{- | For .spl to .st translation "c.at(i, put: j)" means "c at: i put: j".
     In addition "p.q(i, j)" is understood to mean "p q: i value: j".
     That is, if the message name has fewer parts than there are arguments, supply the implicit name "value" for the missing parts.
-}
splDotMessageKeywordSt :: SplDotMessage -> St.KeywordMessage
splDotMessageKeywordSt (SplDotMessage nm arg) =
  let extendWithImplicit = True
      nmParts = Split.splitOn ":" nm
      nmSize = length nmParts
      argSize = length arg
      messageParts =
        if extendWithImplicit && nmSize < argSize
          then take argSize (nmParts ++ repeat splImplicitKeywordName)
          else nmParts
  in if null arg
      then error "splDotMessageKeywordSt: Unary"
      else
        if length messageParts == argSize
          then St.KeywordMessage (zipWith (\i j -> (i ++ ":", splArgumentSt j)) messageParts arg)
          else error "splDotMessageKeywordSt: N-ary messages name degree mismatch"

splDotMessageUnarySt :: SplDotMessage -> St.UnaryMessage
splDotMessageUnarySt (SplDotMessage i a) =
  case a of
    [] -> St.UnaryMessage i
    _ -> error ("splDotMessageUnarySt: argument: " ++ show a)

splMessagesSt :: SplMessages -> St.Messages
splMessagesSt m =
  case m of
    SplMessagesDot m1 m2 ->
      case splDotMessagesForSmalltalk m1 of
        ([], Just k) -> St.MessagesKeyword (splDotMessageKeywordSt k)
        (u, k) ->
          St.MessagesUnary
            (map splDotMessageUnarySt u)
            (fmap (map splBinaryMessageSt) m2)
            (fmap splDotMessageKeywordSt k)
    SplMessagesBinary m1 -> St.MessagesBinary (map splBinaryMessageSt m1) Nothing

splTemporarySt :: SplTemporary -> St.Identifier
splTemporarySt t =
  case t of
    (i, Nothing) -> i
    _ -> error "splTemporarySt"

-- | Temporaries must be in correct form.
splTemporariesSt :: [SplTemporaries] -> St.Temporaries
splTemporariesSt t =
  case t of
    [t1] -> St.Temporaries (map splTemporarySt t1)
    _ -> error "splTemporariesSt"

splReturnStatementSt :: SplReturnStatement -> St.ReturnStatement
splReturnStatementSt (SplReturnStatement e) = St.ReturnStatement (splExpressionSt e)

splStatementsSt :: SplStatements -> St.Statements
splStatementsSt stm =
  case stm of
    SplStatementsReturn x -> St.StatementsReturn (splReturnStatementSt x)
    SplStatementsExpression x y -> St.StatementsExpression (splExpressionSt x) (fmap splStatementsSt y)

splReturnStatement :: SplReturnStatement -> St.ReturnStatement
splReturnStatement (SplReturnStatement x) = St.ReturnStatement (splExpressionSt x)

splBasicExpressionSt :: SplBasicExpression -> St.BasicExpression
splBasicExpressionSt (SplBasicExpression x y) =
  St.BasicExpression (splPrimarySt x) (fmap splMessagesSt y) Nothing

splExpressionSt :: SplExpression -> St.Expression
splExpressionSt e =
  case e of
    SplExprAssignment i e1 -> St.ExprAssignment (St.Assignment i (splExpressionSt e1))
    SplExprBasic e1 -> St.ExprBasic (splBasicExpressionSt e1)

splInitializerDefinitionSt :: SplInitializerDefinition -> St.InitializerDefinition
splInitializerDefinitionSt (SplInitializerDefinition cmt tmp stm) =
  St.InitializerDefinition cmt (fmap splTemporariesSt tmp) (fmap splStatementsSt stm)

-- * Spl translator

{- | Separate out any leading // prefixed comment

>>> splLeadingComment "only\na\nprogram\n"
("","only\na\nprogram\n")

>>> splLeadingComment "// a\n// comment\nthen\na\nprogram\n"
("a\ncomment\n","then\na\nprogram\n")
-}
--splLeadingComment :: String -> (String, String)
--splLeadingComment = bimap (unlines . map (drop 3)) unlines . span (isPrefixOf "// ") . lines

splMaybeParseToSpl :: String -> Either String SplInitializerDefinition
splMaybeParseToSpl x = Spl.Lexer.runAlex x Spl.Parser.parseInitializerDefinition

-- | Spl parse
splParseToSpl :: String -> SplInitializerDefinition
splParseToSpl x =
  case splMaybeParseToSpl x of
    Left l -> error l
    Right r -> r

-- | Sc parse
--scParseToSpl :: String -> SplInitializerDefinition
--scParseToSpl = SuperCollider.Parser.parseInitializerDefinition . Spl.Lexer.alexScanTokens

parseToSt :: (String -> SplInitializerDefinition) -> String -> St.InitializerDefinition
parseToSt f s =
  let (c, p) = splLeadingComment s
      eSpl = splInitializerDefinitionSetComment c (f p)
  in splInitializerDefinitionSt (Spl.Rewrite.splInitializerDefinitionRewrite eSpl)

-- | Parse C-Smalltalk InitializerDefinition.
splParseToSt :: String -> St.InitializerDefinition
splParseToSt = parseToSt splParseToSpl

-- | Parse SuperCollider InitializerDefinition.
--scParseToSt :: String -> St.InitializerDefinition
--scParseToSt = parseToSt scParseToSpl

{- | Translate Spl program text to Smalltalk.

>>> splToSt "(p .. q)"
"((p) to: (q)) .\n"

>>> splToSt "[p .. q]"
"(((p) to: (q)) asArray) .\n"

>>> splToSt "p := q"
"p := q .\n"

>>> splToSt "(p - q).m"
"(p - q) m .\n"

>>> splToSt "p(q.r(i).s).t(j) + k"
"((p apply: {(q r: i) s}) t: j) + k .\n"

>>> splToSt "SinOsc(220,0.5)"
"(SinOsc apply: {220. 0.5}) .\n"

>>> splToSt "x(i) + y(j,k)"
"(x apply: {i}) + (y apply: {j. k}) .\n"

>>> splToSt "x(i, y(j, k))"
"(x apply: {i. (y apply: {j. k})}) .\n"

>>> splToSt "p + q.r.s(a).t.u(b)"
"p + (((q r s: a)) t u: b) .\n"

>>> splToSt "p + q.r + s.t(u) + v()"
"p + q r + (s t: u) + (v apply: {}) .\n"

>>> splToSt "f.value(i, j)"
"f value: i value: j .\n"

>>> splToSt "c.put(i, j)"
"c put: i value: j .\n"

>>> splToSt "p(q.r(i).s).t + k"
"(p apply: {(q r: i) s}) t + k .\n"

>>> splToSt "// commentary\nprogram"
"\"commentary\n\" program .\n"

>>> splToSt "[1, 2, 3, 4, 5]"
"{1. 2. 3. 4. 5} .\n"

>>> splToSt "p.if { q } { r }"
"p if: [ q .\n ] value: [ r .\n ] .\n"

>>> splToSt "(x: p, y: q)"
"(Dictionary newFromPairs: {x. p. y. q}) .\n"

>>> splToSt "p.q * x.f { y }"
"p q * (x f: [ y .\n ]) .\n"

>>> splToSt "p +++ q"
"p +++ q .\n"

>>> splToSt "f(x) { y }"
"(f apply: {x. [ y .\n ]}) .\n"

>>> splToSt "p. /* q */ r"
"p r .\n"

>>> splToSt "f(p: x, q: y)"
"(f p: x q: y) .\n"

>>> splToSt "c.at(i, put: x)"
"c at: i put: x .\n"

>>> splToSt "/* # x\n `$` */ x"
"x .\n"

> splToSt "f.m(p: x, q: y)" -- error
-}
splToSt :: String -> String
splToSt = Ansi.Print.initializerDefinition_pp . splParseToSt

--scToSt :: String -> String
--scToSt = Ansi.Print.initializerDefinition_pp . scParseToSt

{- | Parse .spl and translate to Expr.Expr.

>>> splToExpr "60.MidiCps"
Init (Just "") [] [Send (Literal (NumberLiteral (Int 60))) (Message (UnarySelector "MidiCps") [])]
-}
splToExpr :: String -> Expr.Expr
splToExpr = Expr.initializerDefinitionExpr . splParseToSt

--scToExpr :: String -> Expr.Expr
--scToExpr = Expr.initializerDefinitionExpr . scParseToSt

-- | exprPrintJs of splToExpr
splToJs :: Maybe String -> String -> String
splToJs maybePrefix =
  Expr.Print.exprPrintJs (Expr.Print.jsRenamerFromTable maybePrefix Expr.Print.jsDefaultRenamingTable)
    . splToExpr

-- | exprPrintScheme of splToExpr
splToScheme :: String -> String
splToScheme =
  Expr.Print.exprPrintScheme (Expr.Print.jsRenamerFromTable Nothing Expr.Print.jsDefaultRenamingTable)
    . splToExpr

splToAst :: String -> String
splToAst = show . splToExpr

-- | Statements list of .spl Init Expr.
splToExprStm :: String -> [Expr.Expr]
splToExprStm = Expr.initStatements . splToExpr

{- | Parse and print .spl

>>> splToExprToSpl "(a - b).c"
"(a - b).c"
-}
splToExprToSpl :: String -> String
splToExprToSpl = Expr.Print.exprPrintSpl True . splToExpr

{- | Print .spl as .spl.  Apply is not printed implicitly.

>>> splToSpl "SinOsc(440, 0)"
"SinOsc.apply([440, 0])"

>>> splToSpl "f(x, y)"
"f.apply([x, y])"
-}
splToSpl :: String -> String
splToSpl = Expr.Print.exprPrintSpl False . splToExpr

-- * Spl

{- | Separate out any leading single line block comment.

>>> splLeadingComment "only\na\nprogram\n"
("","only\na\nprogram\n")

>>> splLeadingComment "(* a comment *)\nthen\na\nprogram\n"
("a comment","then\na\nprogram\n")
-}
splLeadingComment :: String -> (String, String)
splLeadingComment x =
  let isComment str = "/* " `isPrefixOf` str && " */" `isSuffixOf` str
      removeComment str = List.dropRight 3 (drop 3 str)
  in case lines x of
      (l1 : l') -> if isComment l1 then (removeComment l1, unlines l') else ("", x)
      _ -> ("", x)

{- | Separate out any leading prefix comment

>>> splLeadingPrefixComment ";;" ";; a\n;; comment\nthen\na\nprogram\n"
("a\ncomment\n","then\na\nprogram\n")
-}
--splLeadingPrefixComment :: String -> String -> (String, String)
--splLeadingPrefixComment prefix =
--  bimap (unlines . map (drop 3)) unlines
--    . span (isPrefixOf (prefix ++ " "))
--    . lines

{- | In Spl f(x, y) and x.f(y) are interchangeable.
In particular f may be a "constructor" procedure.
Spl implements methods for the required cases, following the usual St case rule.
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

-- | Parse Spl to St.
--splParseToSt :: String -> St.InitializerDefinition
--splParseToSt s =
--  let (c, p) = splLeadingComment s
--      p' = splRewriteBinaryOperators (splRewriteDotExpressions p)
--      eSpl = splInitializerDefinitionSetComment c (splParseToSpl p')
--  in splInitializerDefinitionSt (Spl.Rewrite.splInitializerDefinitionRewrite eSpl)

-- | Parse .spl and translate to Init Expr.
--splToExpr :: String -> Expr.Expr
--splToExpr =
--  Expr.initializerDefinitionExpr
--    . splParseToSt

{- | Spl to St

>>> splToSt "x := 0" -- Assignment
"x := 0 .\n"

>>> splToSt "let x = 0; x" -- Let binding
"| x |\n x := 0 .\n x .\n"

>>> splToSt "let x = 0; let y = 0; x + y" -- Let binding
"| x y |\n x := 0 .\n y := 0 .\n x + y .\n"

>>> splToSt "{ :x | x }" -- Block
"[ :x | x .\n ] .\n"

>>> splToSt "{ :f/0 | f() }" -- Block arity qualified argument
"[ :f/0 | (f apply: {}) .\n ] .\n"

>>> splToSt "f(x, y)" -- Apply syntax
"(f apply: {x. y}) .\n"

>>> splToSt "x" -- Variable reference
"x .\n"

>>> splToSt "+" -- Operator reference
"+ .\n"

>>> splToSt "f/1" -- Arity-qualified variable reference
"f/1 .\n"

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

>>> splToSt "(:)" -- Empty dictionary
"(Dictionary newFromPairs: {}) .\n"

>>> splToSt "(k: v)" -- Dictionary
"(Dictionary newFromPairs: {k. v}) .\n"

>>> splToSt "i + c::k + j" -- Quoted at syntax
"i + (c at: '''k''') + j .\n"

>>> splToSt "(1)" -- Parenthesised expression
"(1) .\n"

>>> splToSt "(1, 2)" -- Tuple expression
"({1. 2} asTuple) .\n"

>>> splToSt "L[1,2,3]" -- List constructor syntax
"(L apply: {{1. 2. 3}}) .\n"

>>> splToSt "L[1 2 3]" -- List constructor syntax
"(L apply: {{1. 2. 3}}) .\n"

>>> splToSt "L[1 2 3; 4 5 6]" -- List constructor syntax
"(L apply: {{{1. 2. 3}. {4. 5. 6}}}) .\n"
-}
--splToSt :: String -> String
--splToSt = Ansi.Print.initializerDefinition_pp . splParseToSt

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

{- | Translate Spl method to St.
Rewrites temporaries and for precedence.
The class method "constructor" is special.
It's pattern is derived from it's arguments names, and it generates a primary factory method.
-}
splMethodDefinitionToSt :: St.Identifier -> SplMethodDefinition -> [St.MethodDefinition]
splMethodDefinitionToSt cl_nm md =
  let is_cl = isClassMethod md
      st_cl_nm = if is_cl then cl_nm ++ " class" else cl_nm
      blk = Spl.Rewrite.splBlockBodyRewrite (methodBody md)
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
      tmp = fmap splTemporariesSt (blockTemporaries blk)
      stm = fmap splStatementsSt (blockStatements blk)
      st_md = St.MethodDefinition (st_cl_nm, is_cl) Nothing pat tmp stm Nothing Nothing Nothing
  in if is_constructor
      then [st_md, stPrimaryFactoryMethod (st_cl_nm, is_cl) (if null blk_args then "new" else concatMap (++ ":") blk_args)]
      else [st_md]

-- | Translate Spl class to St.
splClassDefinitionToSt :: SplClassDefinition -> St.ClassDefinition
splClassDefinitionToSt cd =
  let nm = className cd
      (cm, im) = splClassDefinitionPartitionMethods cd
  in St.ClassDefinition
      nm
      (superclassName cd)
      St.noInstanceState
      (maybe [] (map fst) (classInstanceVariableNames cd))
      (maybe [] (map fst) (classVariableNames cd))
      []
      (concatMap (splMethodDefinitionToSt nm) im)
      (concatMap (splMethodDefinitionToSt nm) cm)
      Nothing
      (classCategory cd)
      (classComment cd)

{-

rw = splToSt
splToSt "p.q(r: i)" == "p q: {#'r:' -> i} .\n"
splToSt "p.q:r(i)" -- error ; this is an error in Spl and is reported here but with an odd message
splToSt "p.q()" -- error ; this is dis-allowed

-}
