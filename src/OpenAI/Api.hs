module OpenAI.Api where

import Control.Exception (throwIO)
import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Network.HTTP.Client 
  ( HttpException(..), 
    Manager, 
    defaultManagerSettings, 
    newManager, 
    parseRequest, 
    httpLbs, 
    responseBody, 
    method,
    requestHeaders,
    requestBody,
    RequestBody (..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (HeaderName, hAccept, hContentType, hAuthorization)
import Network.HTTP.Types.Method (StdMethod (..), renderStdMethod)
import OpenAI.Types
import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)

class OpenAIEndpoint req where 
    type OpenAIResponse req
    
    httpMethod :: req -> StdMethod

    url :: req -> Text

    reqBody :: req -> Maybe req
    reqBody = Just

    reqDomain :: req -> Text
    reqDomain _ = "api.openai.com/v1"

    reqApiKey :: req -> IO Text
    reqApiKey _ = do 
        loadFile defaultConfig
        maybeApiKey <- lookupEnv "OPENAI_API_KEY"
        case maybeApiKey of 
            Nothing -> error "OPENAI_API_KEY environment variable not found"
            Just apiKey -> pure $ pack apiKey

instance OpenAIEndpoint GetModelsRequest where 
    type OpenAIResponse GetModelsRequest = GetModelsResponse

    httpMethod _ = GET
    url _ = "/models"
    reqBody _ = Nothing

-- | Call the OpenAI API with the given request.
makeOpenAIRequest :: 
    ( OpenAIEndpoint req 
    , FromJSON (OpenAIResponse req)
    , ToJSON req
    ) => 
    req -> 
    IO (OpenAIResponse req)
makeOpenAIRequest req = do
    manager <- newManager tlsManagerSettings
    apiKey <- reqApiKey req
    let reqUrl = unpack $ (reqDomain req) <> url req
    initReq <- parseRequest reqUrl

    let httpReq = initReq
            { method = renderStdMethod (httpMethod req)
            , requestHeaders = [ (hAccept, "application/json")
                               , (hContentType, "application/json")
                               , (hAuthorization, "Bearer " <> encodeUtf8 apiKey)
                               ]
            , requestBody = case reqBody req of
                Nothing -> mempty
                Just body -> RequestBodyLBS $ encode body
            }

    response <- httpLbs httpReq manager
    let decoded = eitherDecode $ responseBody response
    case decoded of
        Left err -> throwIO $ InvalidJsonException err
        Right val -> pure val

