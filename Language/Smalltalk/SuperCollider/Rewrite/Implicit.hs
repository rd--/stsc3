-- | Implicit message re-writing.
module Language.Smalltalk.SuperCollider.Rewrite.Implicit where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import Language.Smalltalk.SuperCollider.Ast {- stsc3 -}

-- | Construct a "keyword" dot message given receiver, selector and arguments.
sc_keyword_message_basic_expression :: St.Identifier -> St.Identifier -> [ScBasicExpression] -> ScBasicExpression
sc_keyword_message_basic_expression rcv sel arg =
  let kw = ScKeywordArgument Nothing
  in ScBasicExpression (ScPrimaryIdentifier rcv) (Just (ScMessagesDot [ScDotMessage sel (map kw arg)] Nothing))

-- | Table mapping receivers to selector for apply, is "SinOsc" to "freq:phase:".
type ScApplySelectorTable = [(St.Identifier, St.Identifier)]

-- | If p is an implicit send, and if the receiver is in the table, rewrite as keyword message.
sc_rewrite_implicit :: ScApplySelectorTable -> ScPrimary -> ScPrimary
sc_rewrite_implicit tbl p =
  case p of
    ScPrimaryImplicitMessageSend rcv arg ->
      case lookup rcv tbl of
        Just sel -> ScPrimaryExpression (ScExprBasic (sc_keyword_message_basic_expression rcv sel arg))
        _ -> p
    _ -> p
