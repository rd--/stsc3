-- | A tree Expr type for Smalltalk expressions.
module Language.Smalltalk.Ansi.Expr where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

{- | A message is a selector and zero or more parameters.
     If the selector is Unary there must be zero parameters.
     If the selector is Binary there must be one parameter.
     If the selector is Keyword there must be as many parameters as the selector indicates.
-}
data Message = Message St.Selector [Expr]

{- | A standard functional Expression type.
     Send replaces Apply.
     Block is named Lambda.
     There are both literal and expression arrays.
-}
data Expr =
    Identifier St.Identifier
  | Literal St.Literal
  | Assignment St.Identifier Expr
  | Return Expr
  | Send Expr Message
  | Lambda [St.BlockArgument] St.Temporaries [Expr]
  | Array [Expr]

primaryExpr :: St.Primary -> Expr
primaryExpr p =
  case p of
    St.PrimaryIdentifier x -> Identifier x
    St.PrimaryLiteral x -> Literal x
    St.PrimaryBlock x -> blockBodyExpr x
    St.PrimaryExpression x -> expressionExpr x
    St.PrimaryArrayExpression x -> Array (map basicExpressionExpr x)

blockBodyExpr :: St.BlockBody -> Expr
blockBodyExpr (St.BlockBody a t s) =
  Lambda
  (maybe [] id a)
  (maybe St.emptyTemporaries id t)
  (maybe [] statementsExprList s)

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
  case (mu,mb) of
    (Nothing,Nothing) -> primaryExpr p
    (Just u,Just b) -> unaryBinaryMessagesExpr (primaryExpr p) u b
    (Just u,Nothing) -> unaryMessagesExpr (primaryExpr p) u
    (Nothing,Just b) -> binaryMessagesExpr (primaryExpr p) b

keywordMessageExpr :: Expr -> St.KeywordMessage -> Expr
keywordMessageExpr e (St.KeywordMessage k) =
  let s = St.KeywordSelector (concatMap fst k)
      p = map (keywordArgumentExpr . snd) k
  in Send e (Message s p)

unaryBinaryKeywordMessagesExpr :: Expr -> [St.UnaryMessage] -> [St.BinaryMessage] -> St.KeywordMessage -> Expr
unaryBinaryKeywordMessagesExpr e u b = keywordMessageExpr (unaryBinaryMessagesExpr e u b)

-- | Cascaded message sends not implemented.
basicExpressionExpr :: St.BasicExpression -> Expr
basicExpressionExpr (St.BasicExpression p m c) =
  case (m,c) of
    (Nothing,Nothing) -> primaryExpr p
    (Just (St.MessagesUnary u Nothing Nothing),Nothing) -> unaryMessagesExpr (primaryExpr p) u
    (Just (St.MessagesUnary u (Just b) Nothing),Nothing) -> unaryBinaryMessagesExpr (primaryExpr p) u b
    (Just (St.MessagesUnary u Nothing (Just k)),Nothing) -> unaryKeywordMessagesExpr (primaryExpr p) u k
    (Just (St.MessagesUnary u (Just b) (Just k)),Nothing) -> unaryBinaryKeywordMessagesExpr (primaryExpr p) u b k
    (Just (St.MessagesBinary b Nothing),Nothing) -> binaryMessagesExpr (primaryExpr p) b
    (Just (St.MessagesBinary b (Just k)),Nothing) -> keywordMessageExpr (binaryMessagesExpr (primaryExpr p) b) k
    (Just (St.MessagesKeyword k),Nothing) -> keywordMessageExpr (primaryExpr p) k
    (_,Just _) -> error "cascaded message sends not implemented"

expressionExpr :: St.Expression -> Expr
expressionExpr e =
  case e of
     St.ExprAssignment (St.Assignment i e') -> Assignment i (expressionExpr e')
     St.ExprBasic e' -> basicExpressionExpr e'

statementsExprList :: St.Statements -> [Expr]
statementsExprList s =
  case s of
    St.StatementsReturn (St.ReturnStatement e) -> [Return (expressionExpr e)]
    St.StatementsExpression e Nothing -> [expressionExpr e]
    St.StatementsExpression e (Just s') -> expressionExpr e : statementsExprList s'
