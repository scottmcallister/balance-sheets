{-# LANGUAGE DeriveGeneric #-}

module Model(User) where

import GHC.Generics

data User = User
  { userId        :: Maybe Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)