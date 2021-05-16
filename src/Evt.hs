{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Evt where

import           Imports
import           Evt.StreamName

-- Messaging
{-
id UUID NOT NULL DEFAULT gen_random_uuid()
type text NOT NULL,
stream_name text NOT NULL,
metadata jsonb,
data jsonb,
position bigint NOT NULL,
global_position bigserial NOT NULL,
time TIMESTAMP WITHOUT TIME ZONE DEFAULT (now() AT TIME ZONE 'utc') NOT NULL,
-}
data MessageData = 
  MessageData { _messageId      :: !UUID
              , _messageType    :: !Text
              , _streamName     :: !Text
              , _metaData       :: !Value
              , _data           :: !Value
              , _position       :: !Int
              , _globalPosition :: !Int
              , _time           :: !UTCTime
              } deriving (Show)

data MessageMetaData = 
  MessageMetaData {

                  } deriving (Show)


-- MessageStore
class ToMessageData t where
  toMessageData :: t -> MessageData

-- instance (ToJSON t) => ToMessageData t where
--     toMessageData _ = MessageData {}

type Version = Int

initialVersion :: Version
initialVersion = (-1)

class (Get m, Put m, HasCategory m) => MessageStore m where

  writeToStore :: (ToMessageData md) => md -> Id -> Maybe Version -> m ()

  write :: (ToMessageData md) => md -> Id -> m ()
  write toMessageData' streamId = 
    writeToStore toMessageData' streamId Nothing

  writeVersion :: (ToMessageData md) => md -> Id -> Version -> m ()
  writeVersion toMessageData' streamId version = 
    writeToStore toMessageData' streamId (Just version)
  
  writeInitial :: (ToMessageData md) => md -> Id -> m ()
  writeInitial toMessageData' streamId = 
    writeToStore toMessageData' streamId (Just initialVersion)
  
class HasCategory m where
  category :: m Category
  
type BatchSize = Int

class Get m where
  get :: Stream -> Position -> BatchSize -> m [MessageData]
  getLast :: Stream -> m (Maybe MessageData)
  
class Put m where
  put :: Stream -> MessageData -> m ()
  putMany :: Stream -> [MessageData] -> m ()

class Projection p where
  type Entity p
  empty :: p (Entity p)
  apply :: Entity p -> MessageData -> p (Entity p)


-- EventStore
type Position = Int

class ( PositionStore m
      , HasCategory m
      , Get m
      , EntityCacheVolatile m
      , EntityCachePersistent m
      , Projection m
      ) => EntityStore m where

  streamNameConfig :: m StreamNameConfig
  streamNameConfig = categoryStream <$> category
  
  fetch :: Id -> m (Entity m)
  fetch id = do
    mEntityInfo <- cachedEntity
    -- retreive entity or create empty
    (entity, entityVersion) <- case mEntityInfo of
                                  Nothing -> do
                                    entity <- empty
                                    pure (entity, initialVersion)
                                  Just info ->
                                    pure info
    -- start at entity postition and continuously grab groups of MessageData
    (entity', entityVersion') <- refresh entity id entityVersion
    -- until we reach the end apply each MessageData to the entity
    -- cache in volatile storage all the time
    -- every so often cache in persistent storage
    -- return entity (and sometimes version?)
    error "do this"
    where
      cachedEntity :: m (Maybe (a, Position))
      cachedEntity =
        runMaybeT $ MaybeT (retrieveVolatile id) <|> MaybeT (retrievePersistent id)

  refresh :: Entity m -> Id -> Version -> m (Entity m, Version)
  refresh entity id version = do
    -- calculate stream name
    streamNameConfig' <- addId id <$> streamNameConfig
    let streamName = toStreamName streamNameConfig'
    let batchSize = error "do this"
    applyStream streamName batchSize entity version

applyStream :: ( Monad m
               , Get m
               , Projection m
               ) => Stream -> Int -> Entity m -> Version -> m (Entity m, Version)
applyStream streamName batchSize entity entityVersion = do
  messages <- get streamName entityVersion batchSize
  if null messages then
    pure (entity, entityVersion)
  else do
    (entity', entityVersion') <- applyMessages messages entity entityVersion
    applyStream streamName batchSize entity' entityVersion'

applyMessages :: ( Monad m
                 , Projection m
                 ) => [ MessageData ] -> Entity m -> Version -> m (Entity m, Version)
applyMessages [] entity entityVersion = pure (entity, entityVersion)
applyMessages (message:messages) entity entityVersion = do
  entity' <- apply entity message
  let entityVersion' = error "do this"
  applyMessages messages entity' entityVersion'




class EntityCacheVolatile m where
  cacheVolatile :: Id -> a -> Position -> m ()
  retrieveVolatile :: Id -> m (Maybe (a, Position))

class EntityCachePersistent m where
  cachePersistent :: Id -> a -> Position -> m ()
  retrievePersistent :: Id -> m (Maybe (a, Position))

-- PositionStore

positionCategoryType :: CategoryType
positionCategoryType = "position"

class (HasCategory m, Monad m) => PositionStore m where
  positionStreamConfig :: m StreamNameConfig
  positionStreamConfig = do
    config <- categoryStream <$> category
    let positionConfig = config & addCategoryType positionCategoryType
    mPositionStoreId <- positionStoreId
    case mPositionStoreId of
      Nothing ->
        pure positionConfig
      Just positionStoreId' ->
        pure $ positionConfig & addId positionStoreId'

  positionStoreId :: m (Maybe Id)
  positionStoreId = pure Nothing

  record :: Int -> m ()
  position :: m (Maybe Position)

-- Calculate position stream


-- Consumer



-- ConsumerGroup ?
-- Component
-- ComponentHost
