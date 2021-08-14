{- | Ansi Ast folding functions.
     FoldPrimary traverses the Ast treating visiting Primary nodes as leaf nodes.
     Primary nodes that have structure (Blocks, Expressions and Arrays) are both traversed and visited.
-}
module Language.Smalltalk.Ansi.Fold where

import Language.Smalltalk.Ansi {- stsc3 -}

-- * Fold Primary

methodDefinitionFoldPrimary :: (st -> Primary -> st) -> st -> MethodDefinition -> st
methodDefinitionFoldPrimary rw st m =
  maybe st (statementsFoldPrimary rw st) (methodStatements m)

statementsFoldPrimary :: (st -> Primary -> st) -> st -> Statements -> st
statementsFoldPrimary rw st stm =
  case stm of
    StatementsReturn ret ->
      returnStatementFoldPrimary rw st ret
    StatementsExpression expr mstm ->
      let st' = expressionFoldPrimary rw st expr
      in maybe st' (statementsFoldPrimary rw st') mstm

returnStatementFoldPrimary :: (st -> Primary -> st) -> st -> ReturnStatement -> st
returnStatementFoldPrimary rw st (ReturnStatement expr) =
  expressionFoldPrimary rw st expr

expressionFoldPrimary :: (st -> Primary -> st) -> st -> Expression -> st
expressionFoldPrimary rw st expr =
  case expr of
    ExprAssignment as -> assignmentFoldPrimary rw st as
    ExprBasic be -> basicExpressionFoldPrimary rw st be

assignmentFoldPrimary :: (st -> Primary -> st) -> st -> Assignment -> st
assignmentFoldPrimary rw st (Assignment _ expr) =
  expressionFoldPrimary rw st expr

basicExpressionFoldPrimary :: (st -> Primary -> st) -> st -> BasicExpression -> st
basicExpressionFoldPrimary rw st (BasicExpression p m cm) =
  let st' = primaryFoldPrimary rw st p
      st'' = maybe st' (messagesFoldPrimary rw st') m
  in maybe st'' (cascadedMessagesFoldPrimary rw st'') cm

primaryFoldPrimary :: (st -> Primary -> st) -> st -> Primary -> st
primaryFoldPrimary rw st p =
  case p of
    PrimaryBlock b ->
      let st' = blockBodyFoldPrimary rw st b in rw st' p
    PrimaryExpression e ->
      let st' = expressionFoldPrimary rw st e in rw st' p
    PrimaryArrayExpression a ->
      let st' = foldl (basicExpressionFoldPrimary rw) st a in rw st' p
    _ -> rw st p

blockBodyFoldPrimary :: (st -> Primary -> st) -> st -> BlockBody -> st
blockBodyFoldPrimary rw st b =
  maybe st (statementsFoldPrimary rw st) (blockStatements b)

messagesFoldPrimary :: (st -> Primary -> st) -> st -> Messages -> st
messagesFoldPrimary rw st m =
  case m of
    MessagesUnary _ b k ->
      let st' = maybe st (foldl (binaryMessageFoldPrimary rw) st) b
      in maybe st' (keywordMessageFoldPrimary rw st') k
    MessagesBinary b k ->
      let st' = foldl (binaryMessageFoldPrimary rw) st b
      in maybe st' (keywordMessageFoldPrimary rw st') k
    MessagesKeyword k ->
      keywordMessageFoldPrimary rw st k

binaryMessageFoldPrimary :: (st -> Primary -> st) -> st -> BinaryMessage -> st
binaryMessageFoldPrimary rw st (BinaryMessage _ a) =
  binaryArgumentFoldPrimary rw st a

binaryArgumentFoldPrimary :: (st -> Primary -> st) -> st -> BinaryArgument -> st
binaryArgumentFoldPrimary rw st (BinaryArgument p _) =
  primaryFoldPrimary rw st p

keywordMessageFoldPrimary :: (st -> Primary -> st) -> st -> KeywordMessage -> st
keywordMessageFoldPrimary rw st (KeywordMessage k) =
  foldl (keywordArgumentFoldPrimary rw) st (map snd k)

keywordArgumentFoldPrimary :: (st -> Primary -> st) -> st -> KeywordArgument -> st
keywordArgumentFoldPrimary rw st (KeywordArgument p _ b) =
  let st' = primaryFoldPrimary rw st p
  in maybe st' (foldl (binaryMessageFoldPrimary rw) st') b

cascadedMessagesFoldPrimary :: (st -> Primary -> st) -> st -> CascadedMessages -> st
cascadedMessagesFoldPrimary rw st cm = foldl (messagesFoldPrimary rw) st cm
