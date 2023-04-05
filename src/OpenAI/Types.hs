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

type OpenAIAPI = Header "Authorization" Text :> "v1" :> "models" :> Get '[JSON] [OpenAIModel]

