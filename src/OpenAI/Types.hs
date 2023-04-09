{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Types where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Prelude

newtype OpenAIExogenousId = OpenAIExogenousId Text
    deriving newtype (Eq, Ord, Read, Show, ToJSON, FromJSON)

instance ToHttpApiData OpenAIExogenousId where
  toUrlPiece (OpenAIExogenousId text) = text

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

-- | GET /v1/models
type GetModels = Header "Authorization" Text :> "v1" :> "models" :> Get '[JSON] [OpenAIModel]

-- | GET /v1/models/:model_id
type GetModelById = Header "Authorization" Text :> "v1" :> "models" :> Capture "model_id" OpenAIExogenousId :> Get '[JSON] OpenAIModel

-- | The OpenAI API
-- type OpenAIAPI = Header "Authorization" Text :> (GetModels :<|> GetModelById)
type OpenAIAPI = 
    (
    GetModels :<|> 
    GetModelById
    )
