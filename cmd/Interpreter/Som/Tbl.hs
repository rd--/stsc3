{- | Tbl is an indexable mutable association list (zero-indexed).
     Indices are Int.
-}
module Interpreter.Som.Tbl where

import Control.Monad.IO.Class {- base -}

import qualified Data.Vector as Vector {- vector -}

import Interpreter.Som.Ref
import Interpreter.Som.Vec

type Table k v = Vec (k,Ref v)

tblSize :: Table k v -> Int
tblSize = Vector.length

tblAt :: MonadIO m => Table k v -> Int -> m v
tblAt tbl ix = deRef (snd (tbl Vector.! ix))

tblAtDefault :: MonadIO m => Table k v -> Int -> m v -> m v
tblAtDefault tbl ix def = if ix < 0 || ix >= tblSize tbl then def else tblAt tbl ix

tblAtPut :: MonadIO m => Table k v -> Int -> v -> m v
tblAtPut tbl ix o = rwRef (snd (tbl Vector.! ix)) (const o) >> return o

tblAtPutDefault :: MonadIO m => Table k v -> Int -> v -> m v -> m v
tblAtPutDefault tbl ix o def =
  if ix < 0 || ix >= tblSize tbl
  then def
  else tblAtPut tbl ix o

tblKeyIndex :: Eq k => Table k v -> k -> Maybe Int
tblKeyIndex tbl key = Vector.findIndex ((== key) . fst) tbl

tblAtKeyMaybe :: (MonadIO m, Eq k) => Table k v -> k -> m (Maybe v)
tblAtKeyMaybe tbl key = maybe (return Nothing) (fmap Just . tblAt tbl) (tblKeyIndex tbl key)

tblAtKeyDefault :: (MonadIO m, Eq k) => Table k v -> k -> m v -> m v
tblAtKeyDefault tbl key def =
  case tblKeyIndex tbl key of
    Just ix -> tblAt tbl ix
    _ -> def

tblAtKeyPutMaybe :: (MonadIO m, Eq k) => Table k v -> k -> v -> m (Maybe v)
tblAtKeyPutMaybe tbl key o =
  case tblKeyIndex tbl key of
    Nothing -> return Nothing
    Just ix -> fmap Just (tblAtPut tbl ix o)

tblFromList :: MonadIO m => [(k,v)] -> m (Table k v)
tblFromList lst = do
  let (keys,values) = unzip lst
  valuesRef <- mapM toRef values
  return (Vector.fromList (zip keys valuesRef))

tblToList :: MonadIO m => Table k v -> m [(k,v)]
tblToList = mapM (\(key,ref) -> deRef ref >>= \value -> return (key,value)) . Vector.toList

{-
tblKeys :: Table k v -> [k]
tblKeys = map fst . Vector.toList

tblAtMaybe :: MonadIO m => Table k v -> Int -> m (Maybe v)
tblAtMaybe tbl ix = if ix < 0 || ix >= tblSize tbl then return Nothing else fmap Just (tblAt tbl ix)

tblHasKey :: Table k v -> St.Identifier -> Bool
tblHasKey tbl = isJust . tblKeyIndex tbl

tblAtKey :: MonadIO m => Table k v -> St.Identifier -> m v
tblAtKey tbl key = tblAtKeyDefault tbl key (error "tblAtKey")
-}
