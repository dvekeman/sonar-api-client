{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Formatter.Csv where

import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import qualified Data.Text as T
import           Data.Char (ord)
import           Data.Csv
import qualified Data.Csv.Incremental as I

import           System.IO

import GHC.Generics

data CsvRule = CsvRule 
  { name :: !T.Text 
  , severity :: !T.Text
  , description :: !T.Text
  } deriving (Eq, Show, Generic)
instance FromNamedRecord CsvRule
instance ToNamedRecord CsvRule
instance DefaultOrdered CsvRule

writeRules :: [CsvRule] -> IO ()
writeRules rules = do
  h <- openFile "rules.csv" WriteMode
  BL.hPut h $ I.encodeDefaultOrderedByNameWith options (go $ map cleanRule rules)
  hClose h
  where
    go = foldr ((<>) . I.encodeNamedRecord) mempty 

options :: EncodeOptions
options = defaultEncodeOptions 
            { encDelimiter = fromIntegral (ord ';')
            , encIncludeHeader = True
            , encQuoting = QuoteAll}

cleanRule :: CsvRule -> CsvRule
cleanRule rule@CsvRule{..} = 
  rule { description = removeLineBreaks description }

removeLineBreaks :: T.Text -> T.Text
removeLineBreaks = T.concat . T.lines