{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Api
import           Model.RuleInfo (RuleDetails(..))
import qualified Formatter.Csv      as Csv
import qualified Formatter.Markdown as MD

import           System.Environment (getArgs)

main :: IO ()
main = do 
  args <- getArgs
  let domain = head args
  rules <- fetchProfileRules domain
--   let csvRules = map toCsvRule rules
--   Csv.writeRules csvRules 
  let mdRules = map toMDRule rules
  MD.writeRules mdRules

toCsvRule :: RuleDetails -> Csv.CsvRule
toCsvRule RuleDetails{..} = Csv.CsvRule name severity htmlDesc

toMDRule :: RuleDetails -> MD.Rule
toMDRule RuleDetails{..} = MD.Rule "" name severity mdDesc