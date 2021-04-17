module Evt where

import           Imports

-- Messaging
data MessageData = 
    MessageData {

    } deriving (Show)

data MessageMetaData = 
    MessageMetaData {

    } deriving (Show)

type Category = Text
type CategoryType = Text
type CategoryTypes = [CategoryType]
type Id = Text
type Ids = [Id]
type Stream = Text

data StreamNameConfig =
    StreamNameConfig {
        _category :: !Text,
        _categoryTypes :: ![Text],
        _ids :: ![Text]
    }

categoryStream :: Category -> StreamNameConfig 
categoryStream category =
    StreamNameConfig category [] []

addId :: Id -> StreamNameConfig -> StreamNameConfig
addId newId (StreamNameConfig category categoryTypes ids) =
    let
        newIds =
            if any (==newId) ids then
                ids
            else
                ids <> [newId]
    in
    StreamNameConfig category categoryTypes newIds

addCategoryType :: CategoryType -> StreamNameConfig -> StreamNameConfig
addCategoryType newCategoryType (StreamNameConfig category categoryTypes ids) =
    let
        newCategoryTypes =
            if any (==newCategoryType) categoryTypes then
                categoryTypes
            else
                sort $ newCategoryType : categoryTypes
    in
    StreamNameConfig category newCategoryTypes ids


streamName :: StreamNameConfig -> Stream
streamName (StreamNameConfig category categoryTypes ids) =
    let
        categoryTypesText = intercalate "+" $ sort categoryTypes
        categoryTypesFull = if length categoryTypes > 0 then
                                ":" <> categoryTypesText
                            else
                                ""
        idsText = intercalate "+" ids
        idsFull = if length ids > 0 then
                    "-" <> idsText
                  else
                    ""
    in
    concat [category, categoryTypesFull, idsFull]

-- MessageStore
-- EventStore
-- PositionStore
-- Consumer
-- ConsumerGroup ?
-- Component
-- ComponentHost