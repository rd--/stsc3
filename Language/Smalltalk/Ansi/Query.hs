-- | Ansi Ast query functions.
module Language.Smalltalk.Ansi.Query where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Fold as St {- stsc3 -}

-- | Print simple summary of a Class definition.
classDefinitionSummary :: St.ClassDefinition -> [String]
classDefinitionSummary c =
  let m pre pp sel = maybe "" ((pre ++) . pp) (sel c)
      l pre pp sel = let w = sel c in if null w then "" else pre ++ unwords (pp w)
  in filter (not . null)
     [  "Class:               " ++ St.className c
     ,m " Superclass:         " id St.superclassName
     ,l " Instance variables: " id St.classInstanceVariableNames
     ,l " Class variables:    " id St.classVariableNames
     ,l " Instance methods:   " (map St.methodSignature) St.instanceMethods
     ,l " Class methods:      " (map St.methodSignature) St.classMethods
     ,m " Class initializer:  " St.initializerDefinition_pp St.classInitializer]

{- | Traverse block body looking for any return statements.
     Note that a block that has a return statement may not execute it.

> p = St.stParse St.blockBody
> map (blockBodyHasReturn . p) ["[]","[^x]","[[^x]]","[{[^x]}]"] == [False,True,True,True]
-}
blockBodyHasReturn :: St.BlockBody -> Bool
blockBodyHasReturn b =
  let primaryIsReturningBlock p =
        case p of
          St.PrimaryBlock x -> St.blockBodyEndsWithReturn x
          _ -> False
      primaries = St.blockBodyFoldPrimary (flip (:)) [] b
  in St.blockBodyEndsWithReturn b || any primaryIsReturningBlock primaries
