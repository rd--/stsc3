-- | Ansi Ast query functions.
module Language.Smalltalk.Ansi.Query where

import Language.Smalltalk.Ansi {- stsc3 -}
import Language.Smalltalk.Ansi.Fold {- stsc3 -}

{- | Traverse block body looking for any return statements.
     Note that a block that has a return statement may not execute it.

> p = stParse blockBody
> map (blockBodyHasReturn . p) ["[]","[^x]","[x. [^x]]","[[[^x]]]","[{[^x]}]"] == [False,True,True,True,True]
-}
blockBodyHasReturn :: BlockBody -> Bool
blockBodyHasReturn b =
  let primaryIsReturningBlock p =
        case p of
          PrimaryBlock x -> blockBodyEndsWithReturn x
          _ -> False
      primaries = blockBodyFoldPrimary (flip (:)) [] b
  in blockBodyEndsWithReturn b || any primaryIsReturningBlock primaries
