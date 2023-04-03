{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Types for https://platform.openai.com/docs/api-reference
module OpenAI.Types where

import Prelude
import Control.Exception (Exception)
import Data.Aeson (ToJSON, toJSON, Value(String), withObject, (.:), parseJSON)
import Data.Aeson.Types (FromJSON, parseJSON, withText)
import Data.Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

newtype OpenAIExogenousId = OpenAIExogenousId Text
    deriving newtype (Eq, Ord, Read, Show, ToJSON, FromJSON)

data OpenAIObject
    = OpenAIObjectList
    | OpenAIObjectModel
    | OpenAIObjectUnknown Text
    deriving (Show)

instance ToJSON OpenAIObject where
  toJSON OpenAIObjectList = "list"
  toJSON OpenAIObjectModel = "model"

instance FromJSON OpenAIObject where
  parseJSON (String v) = case v of
    "list" -> pure OpenAIObjectList
    "model" -> pure OpenAIObjectModel
    other -> OpenAIObjectUnknown <$> parseJSON (String other)
  parseJSON _ = fail "Expected a JSON string"

-- | Model response OpenAI models
data OpenAIModel = OpenAIModel
    {  openAIModelId :: OpenAIExogenousId
    ,  openAIModelObject :: OpenAIObject
    ,  openAIModelOwnedBy :: Text
    }
    deriving (Show, Generic)

instance FromJSON OpenAIModel where
  parseJSON = withObject "OpenAIModel" $ \v ->
    OpenAIModel
      <$> v .: "id"
      <*> v .: "object"
      <*> v .: "owned_by"

-- | GET /api/v1/models request
data GetOpenAIModelsRequest = GetOpenAIModelsRequest
    deriving (Show, Generic, ToJSON)

-- | GET /api/v1/models response
data GetOpenAIModelsResponse = GetOpenAIModelsResponse
    { getOpenAIModelsResponseData :: [OpenAIModel]
    , getOpenAIModelsResponseObject :: Text    
    } 
    deriving (Show, Generic)

instance FromJSON GetOpenAIModelsResponse where
  parseJSON = withObject "GetModelsResponse" $ \v ->
    GetOpenAIModelsResponse
      <$> v .: "data"
      <*> v .: "object"

-- | GET /api/v1/models{model} request
data GetOpenAIModelRequest = GetOpenAIModelRequest OpenAIExogenousId
    deriving (Show, Generic, ToJSON)

-- | GET /api/v1/models{model} response
data GetOpenAIModelResponse = GetOpenAIModelResponse OpenAIModel
    deriving (Show, Generic, FromJSON)

-- | Exception for invalid JSON
data InvalidJsonException = InvalidJsonException String
    deriving (Show, Typeable)
instance Exception InvalidJsonException

