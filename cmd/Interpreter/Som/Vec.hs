{-# Language FlexibleContexts #-}

{- | Vec is a zero indexed mutable array type.
     VecRef is a mutable reference to a Vec.
     Indices are LargeInteger for SOM.
-}
module Interpreter.Som.Vec where

import Control.Monad {- base -}
import Control.Monad.IO.Class {- base -}
import Data.IORef {- base -}

import qualified Control.Monad.Except as Except {- mtl -}
import qualified Data.Vector as Vector {- vector -}
import qualified Data.Vector.Mutable as Vector.Mutable {- vector -}

import Interpreter.Som.Int {- stsc3 -}
import Interpreter.Som.Ref {- stsc3 -}

type Vec t = Vector.Vector t

type VecRef t = Ref (Vec t)

vecAt :: Vec t -> LargeInteger -> t
vecAt vec ix = vec Vector.! (fromIntegral ix)

vecLength :: Vec t -> LargeInteger
vecLength = fromIntegral . Vector.length

vecBoundsCheck :: Except.MonadError String m => String -> Vec t -> LargeInteger -> m ()
vecBoundsCheck msg vec ix =
  when
    (ix < 0 || ix >= vecLength vec)
    (Except.throwError (msg ++ ": index out of range"))

vecRefWrite :: (MonadIO m, Except.MonadError String m) => Ref (Vec t) -> LargeInteger -> t -> m t
vecRefWrite vecRef ix value = do
  vec <- deRef vecRef
  vecBoundsCheck "vecRefWrite" vec ix
  liftIO
    (writeIORef
      vecRef
      (Vector.modify (\mutVec -> Vector.Mutable.write mutVec (fromIntegral ix) value) vec))
  return value
