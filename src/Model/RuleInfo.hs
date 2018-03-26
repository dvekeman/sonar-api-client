{-# LANGUAGE DeriveGeneric #-}

module Model.RuleInfo where

import           Data.Aeson
import qualified Data.Text as T

import           GHC.Generics

data Rule = Rule 
  { rule :: RuleDetails
--   , actives :: [?] 
  } deriving (Eq, Show, Generic)
instance ToJSON Rule
instance FromJSON Rule

data RuleDetails = RuleDetails
  { key :: T.Text
  , repo :: T.Text
  , name :: T.Text
--   , createdAt :: T.Text
  , htmlDesc :: T.Text
  , mdDesc :: T.Text
  , severity :: T.Text
--   , status :: T.Text
--   , internalKey :: T.Text
--   , isTemplate :: Bool
--   , tags :: [ T.Text ]
--   , sysTags :: [ T.Text ]
--   , lang :: T.Text
--   , langName :: T.Text
--   , params :: [ T.Text ]
--   , defaultDebtRemFnType :: T.Text
--   , defaultDebtRemFnOffset :: T.Text
--   , debtOverloaded :: Bool
--   , debtRemFnType :: T.Text
--   , debtRemFnOffset :: T.Text
--   , remFnType :: T.Text
--   , remFnBaseEffort :: T.Text
  -- , type :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON RuleDetails
instance FromJSON RuleDetails