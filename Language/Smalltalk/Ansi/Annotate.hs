-- | Ast annotation functions.
module Language.Smalltalk.Ansi.Annotate where

import Data.Bifunctor {- base -}

import Language.Smalltalk.Ansi

-- * Method Blocks

-- | Assign blockMethodName to all blocks defined within MethodDefinition.
methodDefinitionAnnotateBlocks :: MethodDefinition -> MethodDefinition
methodDefinitionAnnotateBlocks m =
  let nm = methodName m
  in m {methodStatements = fmap (statementsAnnotateBlocks nm) (methodStatements m)}

statementsAnnotateBlocks :: MethodName -> Statements -> Statements
statementsAnnotateBlocks nm st =
  case st of
    StatementsReturn ret ->
      StatementsReturn (returnStatementAnnotateBlocks nm ret)
    StatementsExpression expr mst ->
      StatementsExpression (expressionAnnotateBlocks nm expr) (fmap (statementsAnnotateBlocks nm) mst)

returnStatementAnnotateBlocks :: MethodName -> ReturnStatement -> ReturnStatement
returnStatementAnnotateBlocks nm (ReturnStatement expr) =
  ReturnStatement (expressionAnnotateBlocks nm expr)

expressionAnnotateBlocks :: MethodName -> Expression -> Expression
expressionAnnotateBlocks nm expr =
  case expr of
    ExprAssignment as -> ExprAssignment (assignmentAnnotateBlocks nm as)
    ExprBasic be -> ExprBasic (basicExpressionAnnotateBlocks nm be)

assignmentAnnotateBlocks :: MethodName -> Assignment -> Assignment
assignmentAnnotateBlocks nm (Assignment k expr) =
  Assignment k (expressionAnnotateBlocks nm expr)

basicExpressionAnnotateBlocks :: MethodName -> BasicExpression -> BasicExpression
basicExpressionAnnotateBlocks nm (BasicExpression p m cm) =
  BasicExpression
  (primaryAnnotateBlocks nm p)
  (fmap (messagesAnnotateBlocks nm) m)
  (fmap (cascadedMessagesAnnotateBlocks nm) cm)

primaryAnnotateBlocks :: MethodName -> Primary -> Primary
primaryAnnotateBlocks nm p =
  case p of
    PrimaryBlock b -> PrimaryBlock (b {blockMethodName = Just nm})
    PrimaryExpression e -> PrimaryExpression (expressionAnnotateBlocks nm e)
    PrimaryArrayExpression a -> PrimaryArrayExpression (map (basicExpressionAnnotateBlocks nm) a)
    _ -> p

messagesAnnotateBlocks :: MethodName -> Messages -> Messages
messagesAnnotateBlocks nm m =
  case m of
    MessagesUnary u b k ->
      MessagesUnary u (fmap (map (binaryMessageAnnotateBlocks nm)) b) (fmap (keywordMessageAnnotateBlocks nm) k)
    MessagesBinary b k ->
      MessagesBinary (map (binaryMessageAnnotateBlocks nm) b) (fmap (keywordMessageAnnotateBlocks nm) k)
    MessagesKeyword k ->
      MessagesKeyword (keywordMessageAnnotateBlocks nm k)

binaryMessageAnnotateBlocks :: MethodName -> BinaryMessage -> BinaryMessage
binaryMessageAnnotateBlocks nm (BinaryMessage k a) =
  BinaryMessage k (binaryArgumentAnnotateBlocks nm a)

binaryArgumentAnnotateBlocks :: MethodName -> BinaryArgument -> BinaryArgument
binaryArgumentAnnotateBlocks nm (BinaryArgument p u) =
  BinaryArgument (primaryAnnotateBlocks nm p) u

keywordMessageAnnotateBlocks :: MethodName -> KeywordMessage -> KeywordMessage
keywordMessageAnnotateBlocks nm (KeywordMessage k) =
  KeywordMessage (map (bimap id (keywordArgumentAnnotateBlocks nm)) k)

keywordArgumentAnnotateBlocks :: MethodName -> KeywordArgument -> KeywordArgument
keywordArgumentAnnotateBlocks nm (KeywordArgument p u b) =
  KeywordArgument (primaryAnnotateBlocks nm p) u (fmap (map (binaryMessageAnnotateBlocks nm)) b)

cascadedMessagesAnnotateBlocks :: MethodName -> CascadedMessages -> CascadedMessages
cascadedMessagesAnnotateBlocks nm cm = map (messagesAnnotateBlocks nm) cm
