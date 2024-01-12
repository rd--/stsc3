-- | Implicit message re-writing.
module Language.Smalltalk.Stc.Rewrite.Implicit where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import Language.Smalltalk.Stc.Ast {- stsc3 -}

-- | Construct a dot message given receiver, selector and arguments.
sc_dot_message_basic_expression :: St.Identifier -> St.Identifier -> [StcBasicExpression] -> StcBasicExpression
sc_dot_message_basic_expression rcv sel arg =
  StcBasicExpression (StcPrimaryIdentifier rcv) (Just (StcMessagesDot [StcDotMessage sel arg] Nothing))

-- | Table mapping receivers to selector for apply, is "SinOsc" to "freq:phase:".
type StcApplySelectorTable = [(St.Identifier, St.Identifier)]

-- | If p is an implicit send, and if the receiver is in the table, rewrite as dot message.
sc_rewrite_implicit :: StcApplySelectorTable -> StcPrimary -> StcPrimary
sc_rewrite_implicit tbl p =
  case p of
    StcPrimaryImplicitMessageSend rcv arg ->
      case lookup rcv tbl of
        Just sel -> StcPrimaryExpression (StcExprBasic (sc_dot_message_basic_expression rcv sel arg))
        _ -> p
    _ -> p
