-- | A tree Expr type for Smalltalk expressions.
module Language.Smalltalk.Ansi.Expr where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

{- | A message is a selector and zero or more parameters.
     If the selector is Unary there must be zero parameters.
     If the selector is Binary there must be one parameter.
     If the selector is Keyword there must be as many parameters as the selector indicates.
-}
data Message =
  Message St.Selector [Expr]
  deriving (Eq, Show)

{- | A standard applicative Expression type.
     Send replaces Apply.
     Block is named Lambda.
     There are both literal and expression arrays.
     Sequences (including cascades) are written as Begin.
-}
data Expr =
    Identifier St.Identifier
  | Literal St.Literal
  | Assignment St.Identifier Expr
  | Return Expr
  | Send Expr Message
  | Lambda [St.BlockArgument] St.Temporaries [Expr]
  | Array [Expr]
  | Begin [Expr]
  | Init St.Temporaries [Expr]
  deriving (Eq, Show)

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

exprSequence :: [Expr] -> Expr
exprSequence l =
  case l of
    [] -> error "exprSequence: empty"
    [e] -> e
    _ -> Begin l

basicExpressionExpr :: St.BasicExpression -> Expr
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

statementsExpr :: St.Statements -> Expr
statementsExpr = exprSequence . statementsExprList

smalltalkProgramExpr :: St.SmalltalkProgram -> Expr
smalltalkProgramExpr x =
  case x of
    St.SmalltalkProgram e -> exprSequence (map programElementExpr e)

programElementExpr :: St.ProgramElement -> Expr
programElementExpr e =
  case e of
    St.ProgramGlobal x -> globalDefinitionExpr x
    St.ProgramInitializer x -> initializerDefinitionExpr x

globalDefinitionExpr :: St.GlobalDefinition -> Expr
globalDefinitionExpr (St.GlobalDefinition k v) =
  Assignment k (maybe (Identifier "nil") initializerDefinitionExpr v)

initializerDefinitionExpr :: St.InitializerDefinition -> Expr
initializerDefinitionExpr (St.InitializerDefinition tmp stm) =
  Init (maybe St.emptyTemporaries id tmp) (maybe [] statementsExprList stm)
