-- | Ansi Ast rewriting functions.
module Language.Smalltalk.Ansi.Rewrite where

import Data.Bifunctor {- base -}

import Language.Smalltalk.Ansi {- stsc3 -}

-- * Rewrite Primary

methodDefinitionRewritePrimary :: (Primary -> Primary) -> MethodDefinition -> MethodDefinition
methodDefinitionRewritePrimary rw m =
  m {methodStatements = fmap (statementsRewritePrimary rw) (methodStatements m)}

statementsRewritePrimary :: (Primary -> Primary) -> Statements -> Statements
statementsRewritePrimary rw st =
  case st of
    StatementsReturn ret ->
      StatementsReturn (returnStatementRewritePrimary rw ret)
    StatementsExpression expr mst ->
      StatementsExpression (expressionRewritePrimary rw expr) (fmap (statementsRewritePrimary rw) mst)

returnStatementRewritePrimary :: (Primary -> Primary) -> ReturnStatement -> ReturnStatement
returnStatementRewritePrimary rw (ReturnStatement expr) =
  ReturnStatement (expressionRewritePrimary rw expr)

expressionRewritePrimary :: (Primary -> Primary) -> Expression -> Expression
expressionRewritePrimary rw expr =
  case expr of
    ExprAssignment as -> ExprAssignment (assignmentRewritePrimary rw as)
    ExprBasic be -> ExprBasic (basicExpressionRewritePrimary rw be)

assignmentRewritePrimary :: (Primary -> Primary) -> Assignment -> Assignment
assignmentRewritePrimary rw (Assignment k expr) =
  Assignment k (expressionRewritePrimary rw expr)

basicExpressionRewritePrimary :: (Primary -> Primary) -> BasicExpression -> BasicExpression
basicExpressionRewritePrimary rw (BasicExpression p m cm) =
  BasicExpression
    (primaryRewritePrimary rw p)
    (fmap (messagesRewritePrimary rw) m)
    (fmap (cascadedMessagesRewritePrimary rw) cm)

primaryRewritePrimary :: (Primary -> Primary) -> Primary -> Primary
primaryRewritePrimary rw p =
  case p of
    PrimaryBlock b -> rw (PrimaryBlock (blockBodyRewritePrimary rw b))
    PrimaryExpression e -> rw (PrimaryExpression (expressionRewritePrimary rw e))
    PrimaryArrayExpression a -> rw (PrimaryArrayExpression (map (basicExpressionRewritePrimary rw) a))
    _ -> rw p

blockBodyRewritePrimary :: (Primary -> Primary) -> BlockBody -> BlockBody
blockBodyRewritePrimary rw b =
  b {blockStatements = fmap (statementsRewritePrimary rw) (blockStatements b)}

messagesRewritePrimary :: (Primary -> Primary) -> Messages -> Messages
messagesRewritePrimary rw m =
  case m of
    MessagesUnary u b k ->
      MessagesUnary u (fmap (map (binaryMessageRewritePrimary rw)) b) (fmap (keywordMessageRewritePrimary rw) k)
    MessagesBinary b k ->
      MessagesBinary (map (binaryMessageRewritePrimary rw) b) (fmap (keywordMessageRewritePrimary rw) k)
    MessagesKeyword k ->
      MessagesKeyword (keywordMessageRewritePrimary rw k)

binaryMessageRewritePrimary :: (Primary -> Primary) -> BinaryMessage -> BinaryMessage
binaryMessageRewritePrimary rw (BinaryMessage k a) =
  BinaryMessage k (binaryArgumentRewritePrimary rw a)

binaryArgumentRewritePrimary :: (Primary -> Primary) -> BinaryArgument -> BinaryArgument
binaryArgumentRewritePrimary rw (BinaryArgument p u) =
  BinaryArgument (primaryRewritePrimary rw p) u

keywordMessageRewritePrimary :: (Primary -> Primary) -> KeywordMessage -> KeywordMessage
keywordMessageRewritePrimary rw (KeywordMessage k) =
  KeywordMessage (map (bimap id (keywordArgumentRewritePrimary rw)) k)

keywordArgumentRewritePrimary :: (Primary -> Primary) -> KeywordArgument -> KeywordArgument
keywordArgumentRewritePrimary rw (KeywordArgument p u b) =
  KeywordArgument (primaryRewritePrimary rw p) u (fmap (map (binaryMessageRewritePrimary rw)) b)

cascadedMessagesRewritePrimary :: (Primary -> Primary) -> CascadedMessages -> CascadedMessages
cascadedMessagesRewritePrimary rw cm = map (messagesRewritePrimary rw) cm
