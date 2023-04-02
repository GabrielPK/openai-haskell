{-# LANGUAGE DeriveGeneric #-}

-- | Types for https://platform.openai.com/docs/api-reference
module OpenAI.Types where

import Control.Exception (Exception)
import Data.Aeson (FromJSON)
import Data.Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- newtype OpenAIObject = OpenAIObject Text
--     deriving newtype (Eq, Ord, Read, Show)
    -- deriving newtype (Eq, FromJSON, ToJSON, Ord, Read, Show, ToHttpApiData, FromHttpApiData)

-- | Model response for GET /api/v1/models
data Model = Model
    {  id :: Text
    ,  object :: Text
    ,  owned_by :: Text
    ,  permission :: [Text]
    }
    deriving (Show, Generic)

instance FromJSON Model

-- | GET /api/v1/models request
data GetModelsRequest = GetModelsRequest

-- | GET /api/v1/models response
data GetModelsResponse = GetModelsResponse
  { models :: [Model]
  } 
  deriving (Show, Generic)

instance FromJSON GetModelsResponse

data InvalidJsonException = InvalidJsonException String
    deriving (Show, Typeable)
instance Exception InvalidJsonException