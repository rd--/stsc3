{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- | A tree Expr type for Smalltalk expressions.
module Language.Smalltalk.Ansi.Expr where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

{- | A message is a selector and zero or more parameters.
     If the selector is Unary there must be zero parameters.
     If the selector is Binary there must be one parameter.
     If the selector is Keyword there must be as many parameters as the selector indicates.
-}
data Message t =
  Message St.Selector [Expr t]
  deriving (Functor, Foldable, Traversable, Eq, Show)

-- | Lambda terms may store their Smalltalk definitions (either blocks or methods)
data LambdaDefinition =
    BlockLambda St.BlockBody
  | MethodLambda St.MethodDefinition
  | NullLambda
  deriving (Eq, Show)

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
     Block and Method returns are written as Return.
     There are both literal and expression arrays.
     Sequences (including cascades) are written as Begin.
-}
data Expr t =
    Identifier St.Identifier
  | Literal St.Literal
  | Assignment St.Identifier (Expr t)
  | Return (Expr t) -- ^ Block (non local) or Method (local) return.
  | Send (Expr t) (Message t)
  | Lambda LambdaDefinition [St.Identifier] St.Temporaries [Expr t] -- ^ Block or Method body.
  | Array [Expr t]
  | Begin [Expr t]
  | Init St.Temporaries [Expr t]
  deriving (Functor, Foldable, Traversable, Eq, Show)

expr_map :: (Expr t -> Expr t) -> Expr t -> Expr t
expr_map f e =
  case e of
    Identifier _ -> f e
    Literal _ -> f e
    Assignment p q -> f (Assignment p (expr_map f q))
    Return p -> f (Return (expr_map f p))
    Send p (Message q r) -> f (Send (expr_map f p) (Message q (map (expr_map f) r)))
    Lambda p q r s -> f (Lambda p q r (map (expr_map f) s))
    Array p -> f (Array (map (expr_map f) p))
    Begin p -> f (Begin (map (expr_map f) p))
    Init p q -> f (Init p (map (expr_map f) q))

-- | Is expression the reservered word "super"?
exprIsSuper :: Expr t -> Bool
exprIsSuper = (==) (Identifier "super")

primaryExpr :: St.Primary -> Expr t
primaryExpr p =
  case p of
    St.PrimaryIdentifier x -> Identifier x
    St.PrimaryLiteral x -> Literal x
    St.PrimaryBlock x -> blockBodyExpr x
    St.PrimaryExpression x -> expressionExpr x
    St.PrimaryArrayExpression x -> Array (map basicExpressionExpr x)

blockBodyExpr :: St.BlockBody -> Expr t
blockBodyExpr blockBody =
  let St.BlockBody _ arg tmp stm = blockBody
  in Lambda
     (BlockLambda blockBody)
     (maybe [] id arg)
     (maybe St.emptyTemporaries id tmp)
     (maybe [] (statementsExprList Return) stm)

methodDefinitionExpr :: St.MethodDefinition -> Expr t
methodDefinitionExpr methodDefinition =
  let (St.MethodDefinition _ _ pat tmp stm _) = methodDefinition
  in Lambda
     (MethodLambda methodDefinition)
     (St.patternArguments pat)
     (maybe St.emptyTemporaries id tmp)
     (maybe [] (statementsExprList Return) stm)

unaryMessagesExpr :: Expr t -> [St.UnaryMessage] -> Expr t
unaryMessagesExpr e u =
  case u of
    [] -> e
    St.UnaryMessage s : u' -> unaryMessagesExpr (Send e (Message (St.UnarySelector s) [])) u'

binaryArgumentExpr :: St.BinaryArgument -> Expr t
binaryArgumentExpr (St.BinaryArgument p m) = unaryMessagesExpr (primaryExpr p) (maybe [] id m)

binaryMessagesExpr :: Expr t -> [St.BinaryMessage] -> Expr t
binaryMessagesExpr e b =
  case b of
    [] -> e
    St.BinaryMessage s a : b' ->
      binaryMessagesExpr (Send e (Message (St.BinarySelector s) [binaryArgumentExpr a])) b'

unaryBinaryMessagesExpr :: Expr t -> [St.UnaryMessage] -> [St.BinaryMessage] -> Expr t
unaryBinaryMessagesExpr e u = binaryMessagesExpr (unaryMessagesExpr e u)

unaryKeywordMessagesExpr :: Expr t -> [St.UnaryMessage] -> St.KeywordMessage -> Expr t
unaryKeywordMessagesExpr e u = keywordMessageExpr (unaryMessagesExpr e u)

keywordArgumentExpr :: St.KeywordArgument -> Expr t
keywordArgumentExpr (St.KeywordArgument p mu mb) =
  case (mu,mb) of
    (Nothing,Nothing) -> primaryExpr p
    (Just u,Just b) -> unaryBinaryMessagesExpr (primaryExpr p) u b
    (Just u,Nothing) -> unaryMessagesExpr (primaryExpr p) u
    (Nothing,Just b) -> binaryMessagesExpr (primaryExpr p) b

keywordMessageExpr :: Expr t -> St.KeywordMessage -> Expr t
keywordMessageExpr e (St.KeywordMessage k) =
  let s = St.KeywordSelector (concatMap fst k)
      p = map (keywordArgumentExpr . snd) k
  in Send e (Message s p)

unaryBinaryKeywordMessagesExpr :: Expr t -> [St.UnaryMessage] -> [St.BinaryMessage] -> St.KeywordMessage -> Expr t
unaryBinaryKeywordMessagesExpr e u b = keywordMessageExpr (unaryBinaryMessagesExpr e u b)

messagesExpr :: Expr t -> St.Messages -> Expr t
messagesExpr e m =
  case m of
    St.MessagesUnary u Nothing Nothing -> unaryMessagesExpr e u
    St.MessagesUnary u (Just b) Nothing -> unaryBinaryMessagesExpr e u b
    St.MessagesUnary u Nothing (Just k) -> unaryKeywordMessagesExpr e u k
    St.MessagesUnary u (Just b) (Just k) -> unaryBinaryKeywordMessagesExpr e u b k
    St.MessagesBinary b Nothing -> binaryMessagesExpr e b
    St.MessagesBinary b (Just k) -> keywordMessageExpr (binaryMessagesExpr e b) k
    St.MessagesKeyword k -> keywordMessageExpr e k

exprSequence :: [Expr t] -> Expr t
exprSequence l =
  case l of
    [] -> error "exprSequence: empty"
    [e] -> e
    _ -> Begin l

basicExpressionExpr :: St.BasicExpression -> Expr t
basicExpressionExpr (St.BasicExpression p m c) =
  let q = case m of
            Nothing -> primaryExpr p
            Just m1 -> messagesExpr (primaryExpr p) m1
      t = case q of
            Send e _ -> e
            _ -> q
  in case c of
       Nothing -> q
       Just mList -> exprSequence (q : map (messagesExpr t) mList)

expressionExpr :: St.Expression -> Expr t
expressionExpr e =
  case e of
     St.ExprAssignment (St.Assignment i e') -> Assignment i (expressionExpr e')
     St.ExprBasic e' -> basicExpressionExpr e'

-- | The returnForm is to allow for distinct MethodReturn and BlockReturn nodes (unused).
statementsExprList :: (Expr t -> Expr t) -> St.Statements -> [Expr t]
statementsExprList returnForm s =
  case s of
    St.StatementsReturn (St.ReturnStatement e) -> [returnForm (expressionExpr e)]
    St.StatementsExpression e Nothing -> [expressionExpr e]
    St.StatementsExpression e (Just s') -> expressionExpr e : statementsExprList returnForm s'

statementsExpr :: (Expr t -> Expr t) -> St.Statements -> Expr t
statementsExpr returnForm = exprSequence . statementsExprList returnForm

smalltalkProgramExpr :: St.SmalltalkProgram -> Expr t
smalltalkProgramExpr x =
  case x of
    St.SmalltalkProgram e -> exprSequence (map programElementExpr e)

programElementExpr :: St.ProgramElement -> Expr t
programElementExpr e =
  case e of
    St.ProgramGlobal x -> globalDefinitionExpr x
    St.ProgramInitializer x -> initializerDefinitionExpr x

globalDefinitionExpr :: St.GlobalDefinition -> Expr t
globalDefinitionExpr (St.GlobalDefinition k v) =
  Assignment k (maybe (Identifier "nil") initializerDefinitionExpr v)

-- | Ansi 3.4.3
initializerDefinitionExpr :: St.InitializerDefinition -> Expr t
initializerDefinitionExpr (St.InitializerDefinition tmp stm) =
  Init
  (maybe St.emptyTemporaries id tmp)
  (maybe [] (statementsExprList Return) stm)
