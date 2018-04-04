{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
       ( startApp
       , app
       ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runStderrLoggingT)
import           Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                                          , runSqlPool, runSqlPersistMPool
                                          , runMigration, selectFirst, (==.)
                                          , insert, entityVal)
import           Data.String.Conversions (cs)
import           Data.Text (Text)
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors

import           Servant

import           Api
import           Models

server :: ConnectionPool -> Server Api
server pool =
  userAddH :<|> userGetH
  where
    userAddH newUser = liftIO $ userAdd newUser
    userGetH name    = liftIO $ userGet name

    userAdd :: User -> IO (Maybe (Key User))
    userAdd newUser = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [UserName ==. userName newUser] []
      case exists of
        Nothing -> Just <$> insert newUser
        Just _ -> return Nothing

    userGet :: Text -> IO (Maybe User)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserName ==. name] []
      return $ entityVal <$> mUser


apiResourcePolicy :: CorsResourcePolicy
apiResourcePolicy =
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

apiCors :: Middleware
apiCors = cors $ const (Just apiResourcePolicy)

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return
    $ apiCors
    $ app pool

startApp :: FilePath -> IO ()
startApp sqliteFile = do
  putStrLn "app running on port 3000"
  s <- mkApp sqliteFile
  Warp.run 3000 =<< mkApp sqliteFile
