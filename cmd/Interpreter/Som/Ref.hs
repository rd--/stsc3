-- | Mutable reference type.
module Interpreter.Som.Ref where

import Control.Monad.IO.Class {- base -}
import Data.IORef {- base -}

-- | Reference.
type Ref = IORef

-- | To reference, liftIO of newIORef
toRef :: MonadIO m => t -> m (Ref t)
toRef = liftIO . newIORef

-- | From reference, liftIO of readIORef
deRef :: MonadIO m => Ref t -> m t
deRef = liftIO . readIORef

-- | Mutate reference, liftIO of modifyIORef
rwRef :: MonadIO m => Ref t -> (t -> t) -> m ()
rwRef r = liftIO . modifyIORef r
