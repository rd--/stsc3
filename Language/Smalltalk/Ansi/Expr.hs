-- | A tree Expr type for Smalltalk expressions.
module Language.Smalltalk.Ansi.Expr where

import Data.List {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

{- | A message is a selector and zero or more parameters.
     If the selector is Unary there must be zero parameters.
     If the selector is Binary there must be one parameter.
     If the selector is Keyword there must be as many parameters as the selector indicates.
-}
data Message
  = Message St.Selector [Expr]
  deriving (Eq, Show)

-- | Lambda terms may store their Smalltalk definitions (either blocks or methods)
data LambdaDefinition
  = BlockLambda St.BlockBody
  | MethodLambda St.MethodDefinition
  | NullLambda
  deriving (Eq, Show)

lambdaDefinitionPrimitive :: LambdaDefinition -> Maybe St.Primitive
lambdaDefinitionPrimitive ld =
  case ld of
    MethodLambda md -> St.methodPrimitive md
    _ -> Nothing

-- | Shows the method name (or Workspace for non-method blocks) and lambda type.
lambdaDefinitionShow :: LambdaDefinition -> String
lambdaDefinitionShow ld =
  case ld of
    BlockLambda b -> "Block from " ++ maybe "Workspace" St.methodNameIdentifier (St.blockMethodName b)
    MethodLambda m -> "Method " ++ St.methodNameIdentifier (St.methodName m)
    NullLambda -> "Lambda"

{- | A standard applicative Expression type.
     Send replaces Apply.
     Block and Method bodies are written as Lambda.
     Lambda has a definition, arguments, temporaries, statements, and a return.
     The final return expression is optional, for blocks it is a non local return, for methods it is a local return.
     There are both literal and expression arrays.
     Programs are written as Init, which is not allowed to have a return statement.
     Cascades are written as Lambda, see cascadeLambda.
-}
data Expr
  = Identifier St.Identifier
  | Literal St.Literal
  | Assignment St.Identifier Expr
  | Send Expr Message
  | Lambda LambdaDefinition [St.Identifier] [St.Identifier] ([Expr], Maybe Expr)
  | Array [Expr]
  | Init (Maybe St.Comment) [St.Identifier] [Expr]
  deriving (Eq, Show)

exprHasDuplicateTemporaries :: Expr -> Bool
exprHasDuplicateTemporaries e =
  case e of
    Lambda _ a t _ -> (length a + length t) == length (nub (a ++ t))
    _ -> False

exprIsAssignment :: Expr -> Bool
exprIsAssignment e =
  case e of
    Assignment {} -> True
    _ -> False

assignmentIdentifier :: Expr -> Maybe St.Identifier
assignmentIdentifier e =
  case e of
    Assignment x _ -> Just x
    _ -> Nothing

expr_map :: (Expr -> Expr) -> Expr -> Expr
expr_map f e =
  case e of
    Identifier _ -> f e
    Literal _ -> f e
    Assignment p q -> f (Assignment p (expr_map f q))
    Send p (Message q r) -> f (Send (expr_map f p) (Message q (map (expr_map f) r)))
    Lambda p q r (s, t) -> f (Lambda p q r (map (expr_map f) s, fmap (expr_map f) t))
    Array p -> f (Array (map (expr_map f) p))
    Init c p q -> f (Init c p (map (expr_map f) q))

-- | Is expression the reservered word "super"?
exprIsSuper :: Expr -> Bool
exprIsSuper = (==) (Identifier "super")

-- | Is Expr a binary operator message send.
exprIsBinaryMessageSend :: Expr -> Bool
exprIsBinaryMessageSend expr =
  case expr of
    Send _lhs (Message (St.BinarySelector _) [_rhs]) -> True
    _ -> False

-- | Is Expr a keyword message.
exprIsKeywordMessageSend :: Expr -> Bool
exprIsKeywordMessageSend expr =
  case expr of
    Send _lhs (Message (St.KeywordSelector _ _) (_ : _)) -> True
    _ -> False

-- | Is Expr a unary message send.
exprIsUnaryMessageSend :: Expr -> Bool
exprIsUnaryMessageSend expr =
  case expr of
    Send _lhs (Message (St.UnarySelector _) []) -> True
    _ -> False

primaryExpr :: St.Primary -> Expr
primaryExpr p =
  case p of
    St.PrimaryIdentifier x -> Identifier x
    St.PrimaryLiteral x -> Literal x
    St.PrimaryBlock x -> blockBodyExpr x
    St.PrimaryExpression x -> expressionExpr x
    St.PrimaryArrayExpression x -> Array (map basicExpressionExpr x)

blockBodyExpr :: St.BlockBody -> Expr
blockBodyExpr blockBody =
  let St.BlockBody _ arg tmp stm = blockBody
  in Lambda
      (BlockLambda blockBody)
      (maybe [] id arg)
      (maybe [] St.temporariesIdentifiers tmp)
      (maybe ([], Nothing) statementsExprList stm)

methodDefinitionExpr :: St.MethodDefinition -> Expr
methodDefinitionExpr methodDefinition =
  let (St.MethodDefinition _ _ pat tmp stm _ _ _) = methodDefinition
  in Lambda
      (MethodLambda methodDefinition)
      (St.patternArguments pat)
      (maybe [] St.temporariesIdentifiers tmp)
      (maybe ([], Nothing) statementsExprList stm)

unaryMessagesExpr :: Expr -> [St.UnaryMessage] -> Expr
unaryMessagesExpr e u =
  case u of
    [] -> e
    St.UnaryMessage s : u' -> unaryMessagesExpr (Send e (Message (St.UnarySelector s) [])) u'

binaryArgumentExpr :: St.BinaryArgument -> Expr
binaryArgumentExpr (St.BinaryArgument p m) = unaryMessagesExpr (primaryExpr p) (maybe [] id m)

binaryMessagesExpr :: Expr -> [St.BinaryMessage] -> Expr
binaryMessagesExpr e b =
  case b of
    [] -> e
    St.BinaryMessage s a : b' ->
      binaryMessagesExpr (Send e (Message (St.BinarySelector s) [binaryArgumentExpr a])) b'

unaryBinaryMessagesExpr :: Expr -> [St.UnaryMessage] -> [St.BinaryMessage] -> Expr
unaryBinaryMessagesExpr e u = binaryMessagesExpr (unaryMessagesExpr e u)

unaryKeywordMessagesExpr :: Expr -> [St.UnaryMessage] -> St.KeywordMessage -> Expr
unaryKeywordMessagesExpr e u = keywordMessageExpr (unaryMessagesExpr e u)

keywordArgumentExpr :: St.KeywordArgument -> Expr
keywordArgumentExpr (St.KeywordArgument p mu mb) =
  case (mu, mb) of
    (Nothing, Nothing) -> primaryExpr p
    (Just u, Just b) -> unaryBinaryMessagesExpr (primaryExpr p) u b
    (Just u, Nothing) -> unaryMessagesExpr (primaryExpr p) u
    (Nothing, Just b) -> binaryMessagesExpr (primaryExpr p) b

keywordMessageExpr :: Expr -> St.KeywordMessage -> Expr
keywordMessageExpr e (St.KeywordMessage k) =
  let s = St.asKeywordSelector (concatMap fst k)
      p = map (keywordArgumentExpr . snd) k
  in Send e (Message s p)

unaryBinaryKeywordMessagesExpr :: Expr -> [St.UnaryMessage] -> [St.BinaryMessage] -> St.KeywordMessage -> Expr
unaryBinaryKeywordMessagesExpr e u b = keywordMessageExpr (unaryBinaryMessagesExpr e u b)

messagesExpr :: Expr -> St.Messages -> Expr
messagesExpr e m =
  case m of
    St.MessagesUnary u Nothing Nothing -> unaryMessagesExpr e u
    St.MessagesUnary u (Just b) Nothing -> unaryBinaryMessagesExpr e u b
    St.MessagesUnary u Nothing (Just k) -> unaryKeywordMessagesExpr e u k
    St.MessagesUnary u (Just b) (Just k) -> unaryBinaryKeywordMessagesExpr e u b k
    St.MessagesBinary b Nothing -> binaryMessagesExpr e b
    St.MessagesBinary b (Just k) -> keywordMessageExpr (binaryMessagesExpr e b) k
    St.MessagesKeyword k -> keywordMessageExpr e k

{- | Cascade message translation.

Cascades send a sequence of messages to the same receiver.
"p q; r; s" sends q to p, and then r to p and then s to p, returning the last answer.
Cascades cannot be re-written as a sequence of expressions without introducing a local variable.
I.e. in "(p q) r; s" s is sent to the answer of "p q", which is unknown after evaluating "(p q) r".
To introduce the binding the rewriting is of the form "[:z | z r. z s] value: (p q)" where z is an invented name.

> p = St.stParse St.basicExpression
> basicExpressionExpr (p "p q; r")
> basicExpressionExpr (p "p q: r; s: t")
> basicExpressionExpr (p "(p q: r) s; t: u")
> basicExpressionExpr (p "p; q") -- error
-}
cascadeLambda :: Message -> St.CascadedMessages -> Expr
cascadeLambda m c =
  let z = "cascadeTemporaryVariable"
      f = messagesExpr (Identifier z)
  in Lambda NullLambda [z] [] (Send (Identifier z) m : map f c, Nothing)

-- | In the case of a cascade, the last message of m must be lifted out and send to cascadeLambda.
basicExpressionExpr :: St.BasicExpression -> Expr
basicExpressionExpr (St.BasicExpression p m c) =
  case m of
    Nothing -> primaryExpr p
    Just m' ->
      let e = messagesExpr (primaryExpr p) m'
      in case c of
          Nothing -> e
          Just c' ->
            case e of
              Send e' m'' -> keywordSend (cascadeLambda m'' c') "value:" [e']
              _ -> error "basicExpressionExpr: cascade a non-Send?"

expressionExpr :: St.Expression -> Expr
expressionExpr e =
  case e of
    St.ExprAssignment (St.Assignment i e') -> Assignment i (expressionExpr e')
    St.ExprBasic e' -> basicExpressionExpr e'

statementsExprList :: St.Statements -> ([Expr], Maybe Expr)
statementsExprList =
  let f (p, q) = (map expressionExpr p, fmap expressionExpr q)
  in f . St.statementsUnfold

statementsExprListNoReturn :: St.Statements -> [Expr]
statementsExprListNoReturn stm =
  case statementsExprList stm of
    (expr, Nothing) -> expr
    _ -> error "Statements has disallowed return?"

-- | Ansi 3.4.3
initializerDefinitionExpr :: St.InitializerDefinition -> Expr
initializerDefinitionExpr (St.InitializerDefinition cmt tmp stm) =
  Init
    cmt
    (maybe [] St.temporariesIdentifiers tmp)
    (maybe [] statementsExprListNoReturn stm)

-- * Expr operators

-- | Init statements, discarding temporaries.
initStatements :: Expr -> [Expr]
initStatements expr =
  case expr of
    Init _ _ x -> x
    _ -> error "initStatements: not init?"

-- | e.m
unarySend :: Expr -> St.Identifier -> Expr
unarySend e m = Send e (Message (St.UnarySelector m) [])

-- | e.m1.m2 &etc. (left to right)
unarySendSequence :: Expr -> [St.Identifier] -> Expr
unarySendSequence e m =
  case m of
    [] -> e
    m1 : m' -> unarySendSequence (unarySend e m1) m'

-- | Symbol literal
symbolLiteral :: St.Symbol -> Expr
symbolLiteral = Literal . St.SymbolLiteral

-- | e.k(l)
keywordSend :: Expr -> St.Symbol -> [Expr] -> Expr
keywordSend e k l = Send e (Message (St.asKeywordSelector k) l)

-- | e(l) -> e.apply([l])
implicitSend :: St.Identifier -> [Expr] -> Expr
implicitSend e l = keywordSend (Identifier e) "apply:" [Array l]

-- | x -> { x }
inLambda :: Expr -> Expr
inLambda x = Lambda NullLambda [] [] ([x], Nothing)

-- * Apply

-- | If Expr is an implicit send (i.e. apply: with an Array argument at an Identifier), get the receiver and arguments.
expr_get_if_implicit :: Expr -> Maybe (St.Identifier, [Expr])
expr_get_if_implicit e =
  case e of
    Send (Identifier rcv) (Message (St.KeywordSelector "apply:" 1) [Array arg]) -> Just (rcv, arg)
    _ -> Nothing

-- | If Expr is an implicit send and if the receiver has a selector in the rewriting table, rewrite as keyword send.
expr_rewrite_if_implicit :: [(String, String)] -> Expr -> Expr
expr_rewrite_if_implicit tbl e =
  case expr_get_if_implicit e of
    Just (rcv, arg) ->
      case lookup rcv tbl of
        Just sel -> keywordSend (Identifier rcv) sel arg
        _ -> e
    _ -> e

-- | Rewrite all implicit sends within an Expr.
exprRewriteImplicit :: [(String, String)] -> Expr -> Expr
exprRewriteImplicit tbl = expr_map (expr_rewrite_if_implicit tbl)
