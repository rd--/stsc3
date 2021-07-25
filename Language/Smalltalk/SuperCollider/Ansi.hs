module Language.Smalltalk.SuperCollider.Ansi where

import Data.Maybe {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

-- | Identifier with perhaps an inializer expression.
type ScTemporary = (St.Identifier,Maybe St.Expression)

type ScTemporaries = [ScTemporary]

-- | Constructor for SuperCollider type block, ie. with direct assignment to variable names.
sc3BlockBody :: Maybe [St.BlockArgument] -> ScTemporaries -> Maybe St.Statements -> St.BlockBody
sc3BlockBody arg tmp stm =
  let tmp_names = map fst tmp
      maybe_tmp = if null tmp_names then Nothing else Just (St.Temporaries tmp_names)
      tmp_exp = mapMaybe (\(k,v) -> fmap (\e -> St.ExprAssignment (St.Assignment k e)) v) tmp
  in case tmp_exp of
       [] -> St.BlockBody arg maybe_tmp stm
       _ -> St.BlockBody arg maybe_tmp (Just (St.expressionSequenceToStatements stm tmp_exp))
