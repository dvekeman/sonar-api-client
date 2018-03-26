{-# LANGUAGE DeriveGeneric #-}

module Model.Profile where

import           Data.Aeson
import qualified Data.Text as T

import           GHC.Generics

data Profile = Profile
  { name :: T.Text
  , language :: T.Text
  -- , _default: Bool
  , rules :: [ProfileRule]
  } deriving (Eq, Show, Generic)
instance ToJSON Profile
instance FromJSON Profile

data ProfileRule = ProfileRule
  { key :: T.Text
  , repo :: T.Text
  , severity :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON ProfileRule
instance FromJSON ProfileRule


