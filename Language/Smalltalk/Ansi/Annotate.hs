-- | Ast annotation functions.
module Language.Smalltalk.Ansi.Annotate where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Rewrite as Rewrite {- stsc3 -}

-- | Assign blockMethodName to all blocks defined within MethodDefinition.
methodDefinitionAnnotateBlocks :: St.MethodDefinition -> St.MethodDefinition
methodDefinitionAnnotateBlocks m =
  let annotate p =
        case p of
          St.PrimaryBlock b -> St.PrimaryBlock (b {St.blockMethodName = Just (St.methodName m)})
          _ -> p
  in Rewrite.methodDefinitionRewritePrimary annotate m
