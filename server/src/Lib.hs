{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Scott" "McAllister"
        , User 2 "Kelly" "McAllister"
        ]


userResourcePolicy :: CorsResourcePolicy
userResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = simpleMethods
    , corsRequestHeaders = simpleHeaders
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

usersCors :: Middleware
usersCors = cors $ const (Just userResourcePolicy)


startApp :: IO ()
startApp = do
  putStrLn "app running on localhost:8080"
  run 8080 $ usersCors app