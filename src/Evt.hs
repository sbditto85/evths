{-# LANGUAGE FlexibleInstances #-}
module Evt where

import           Imports
import           Evt.StreamName

-- Messaging
data MessageData = 
  MessageData {
  
              } deriving (Show)

data MessageMetaData = 
  MessageMetaData {

                  } deriving (Show)


-- MessageStore
type Version = Int

initialVersion ::Version
initialVersion = (-1)

class HasMessageStore m where
  messageStore :: m MessageStore

class ToMessageData t where
  toMessageData :: t -> MessageData

-- instance (ToJSON t) => ToMessageData t where
--     toMessageData _ = MessageData {}

class HasProjection a where
  projection :: Proxy a -> p

class (Get m, Put m, HasCategory m) => MessageStore m where
  write :: (ToMessageData md) => md -> Id -> Maybe Version -> m ()
  writeInitial :: (ToMessageData md) => md -> Id -> m ()
  writeInitial toMessageData' streamId = write toMessageData' streamId (Just initialVersion)
  
class HasCategory m where
  category :: m Category
  
class Get m where
  get :: Stream -> m [MessageData]
  getLast :: Stream -> m (Maybe MessageData)
  
class Put m where
  put :: Stream -> MessageData -> m ()
  putMany :: Stream -> [MessageData] -> m ()

-- EventStore
class (HasMessageStore m) => EntityStore m where
  fetch :: (HasProjection a) => Id -> m a

-- PositionStore
class (HasMessageStore m) => PositionStore m where
  record :: Int -> m ()
  retrieve :: m Int

-- Consumer



-- ConsumerGroup ?
-- Component
-- ComponentHost
