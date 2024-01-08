module Language.Smalltalk.Spl.Ast where

import Language.Smalltalk.SuperCollider.Ast {- stsc3 -}

scMatrixExpression :: [[ScBasicExpression]] -> ScPrimary
scMatrixExpression =
  ScPrimaryArrayExpression
  . map (scPrimaryToBasicExpression . ScPrimaryArrayExpression)
