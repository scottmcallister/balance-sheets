{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson
import Data.Text

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  email Text
  password Text
  UniqueName name
  deriving Eq Read Show
|]

data Login =
    Login
        { name    :: Text
        , password :: Text
        } deriving (Eq, Read, Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \ v ->
    User <$> v .: "name"
         <*> v .: "email"
         <*> v .: "password"

instance ToJSON User where
  toJSON (User name email password) =
    object [ "name" .= name
           , "email" .= email ]
