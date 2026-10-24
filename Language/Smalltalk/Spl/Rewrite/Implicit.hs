-- | Implicit message re-writing.
module Language.Smalltalk.Spl.Rewrite.Implicit where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import Language.Smalltalk.Spl.Ast {- stsc3 -}

-- | Construct a dot message given receiver, selector and arguments.
sc_dot_message_basic_expression :: St.Identifier -> St.Identifier -> [SplBasicExpression] -> SplBasicExpression
sc_dot_message_basic_expression rcv sel arg =
  SplBasicExpression (SplPrimaryIdentifier rcv) (Just (SplMessagesDot [SplDotMessage sel arg] Nothing))

-- | Table mapping receivers to selector for apply, is "SinOsc" to "freq:phase:".
type SplApplySelectorTable = [(St.Identifier, St.Identifier)]

-- | If p is an implicit send, and if the receiver is in the table, rewrite as dot message.
sc_rewrite_implicit :: SplApplySelectorTable -> SplPrimary -> SplPrimary
sc_rewrite_implicit tbl p =
  case p of
    SplPrimaryImplicitMessageSend rcv arg ->
      case lookup rcv tbl of
        Just sel -> SplPrimaryExpression (SplExprBasic (sc_dot_message_basic_expression rcv sel arg))
        _ -> p
    _ -> p
