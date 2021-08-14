-- | Ast annotation functions.
module Language.Smalltalk.Ansi.Annotate where

import Language.Smalltalk.Ansi {- stsc3 -}
import Language.Smalltalk.Ansi.Rewrite {- stsc3 -}

-- | Assign blockMethodName to all blocks defined within MethodDefinition.
methodDefinitionAnnotateBlocks :: MethodDefinition -> MethodDefinition
methodDefinitionAnnotateBlocks m =
  let annotate p =
        case p of
          PrimaryBlock b -> PrimaryBlock (b {blockMethodName = Just (methodName m)})
          _ -> p
  in methodDefinitionRewritePrimary annotate m
