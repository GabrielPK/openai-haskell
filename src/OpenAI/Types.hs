{-# LANGUAGE DeriveGeneric #-}

-- | Types for https://platform.openai.com/docs/api-reference
module OpenAI.Types where

import Prelude
import Control.Exception (Exception)
import Data.Aeson (ToJSON, FromJSON, withObject, (.:), parseJSON)
import Data.Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- newtype OpenAIObject = OpenAIObject Text
--     deriving newtype (Eq, Ord, Read, Show)
    -- deriving newtype (Eq, FromJSON, ToJSON, Ord, Read, Show, ToHttpApiData, FromHttpApiData)

-- | Model response for GET /api/v1/models
data OpenAIModel = OpenAIModel
    {  openAIModelId :: Text
    ,  openAIModelObject :: Text
    ,  openAIModelOwnedBy :: Text
    -- ,  openAIModelPermission :: [Text]
    }
    deriving (Show, Generic)

instance FromJSON OpenAIModel where
  parseJSON = withObject "OpenAIModel" $ \v ->
    OpenAIModel
      <$> v .: "id"
      <*> v .: "object"
      <*> v .: "owned_by"
    --   <*> v .: "permission"

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

-- | Exception for invalid JSON
data InvalidJsonException = InvalidJsonException String
    deriving (Show, Typeable)
instance Exception InvalidJsonException

