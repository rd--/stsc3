module Language.Smalltalk.Spl.Ast where

import Language.Smalltalk.Stc.Ast as Stc {- stsc3 -}

splMatrixExpression :: [[Stc.StcBasicExpression]] -> Stc.StcPrimary
splMatrixExpression =
  Stc.StcPrimaryArrayExpression
    . map (Stc.stcPrimaryToBasicExpression . Stc.StcPrimaryArrayExpression)
