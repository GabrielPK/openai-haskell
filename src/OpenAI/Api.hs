module OpenAI.Api where

import Control.Exception (throwIO)
import Data.Aeson (encode)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException(..), Manager, defaultManagerSettings, newManager, parseRequest, httpLbs, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Method (StdMethod(..))
import OpenAI.Types
import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)

class OpenAIEndpoint req where 
    type OpenAIResponse req
    
    httpMethod :: req -> StdMethod

    url :: Text

    reqBody :: req -> Maybe req
    reqBody = Just

    reqDomain :: Text
    reqDomain = "api.openai.com/v1"

    reqApiKey :: IO Text
    reqApiKey = do 
        loadFile
        maybeApiKey <- lookupEnv "OPENAI_API_KEY"
        case maybeApiKey of 
            Nothing -> error "OPENAI_API_KEY environment variable not found"
            Just apiKey -> pure $ pack apiKey

instance OpenAIEndpoint GetModelsRequest where 
    type OpenAIResponse GetModelsRequest = GetModelsResponse

    httpMethod _ = GET
    url = "/models"
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
    apiKey <- reqApiKey
    let reqUrl = unpack $ reqDomain <> url req
    initReq <- parseRequest reqUrl

    let httpReq = initReq
            { method = httpMethod req
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

