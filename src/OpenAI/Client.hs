module OpenAI.Client where

import Prelude
import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (newTlsManager)
import OpenAI.Types
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)
import qualified Servant.Client.Streaming as S
import System.Environment (lookupEnv)
import Data.Text

openAIDomain :: String
openAIDomain = "api.openai.com"

getOpenAIApiKey :: IO Text 
getOpenAIApiKey = do
    loadFile defaultConfig
    maybeApiKey <- lookupEnv "OPENAI_API_KEY"
    case maybeApiKey of 
        Nothing -> error "OPENAI_API_KEY environment variable not found"
        Just apiKey -> pure $ pack apiKey

api :: Proxy OpenAIAPI
api = Proxy

getModels :<|> getModelById = client api

queries :: Text -> ClientM ([OpenAIModel] , OpenAIModel)
queries apiKey = do
    models <- getModels (Just apiKey)
    model <- getModelById (Just apiKey) (OpenAIExogenousId "text-davinci-003")
    pure (models, model)

run :: IO ()
run = do
    manager' <- newTlsManager
    apiKey <- getOpenAIApiKey
    let baseUrl = BaseUrl Https openAIDomain 443 ""
        bearerKey = "Bearer " <> apiKey
    res <- runClientM (queries bearerKey) (mkClientEnv manager' baseUrl)
    case res of 
        Left err -> putStrLn $ "Error: " ++ show err
        Right (models, model) -> do
            putStrLn "Models:"
            forM_ models print