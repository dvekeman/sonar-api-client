{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Formatter.Markdown where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc

import           System.IO

import GHC.Generics

data Rule = Rule 
  { ruleId   :: !T.Text
  , name :: !T.Text 
  , severity :: !T.Text
  , description :: !T.Text
  } deriving (Eq, Show, Generic)

writeRules :: [Rule] -> IO ()
writeRules rules = do
  h <- openFile "rules.md" WriteMode
  BS8.hPutStrLn h "| Id | Name        | Severity           | Description |"
  BS8.hPutStrLn h "|:--:|:------------|:------------------:|:------------|"
  mapM_ (writeRule h . cleanRule) $ zipWith withId rules [1..]
  hClose h

writeRule :: Handle -> Rule -> IO ()
writeRule h Rule{..} = BS8.hPutStrLn h $ 
                        startTable <> TEnc.encodeUtf8 ruleId <> 
                          midTable <> TEnc.encodeUtf8 name <> 
                          midTable <> TEnc.encodeUtf8 severity <> 
                          midTable <> TEnc.encodeUtf8 description <> 
                        endTable

startTable :: BS.ByteString
startTable = "| "

midTable :: BS.ByteString
midTable = " | "

endTable :: BS.ByteString
endTable = " |"

withId :: Rule -> Int -> Rule
withId r ruleId = r { ruleId = T.pack $ show ruleId }

cleanRule :: Rule -> Rule
cleanRule rule@Rule{..} = 
 rule { description = removeLineBreaks $ breakLongLines description }

removeLineBreaks :: T.Text -> T.Text
removeLineBreaks = 
  T.replace "\n" "<br/>"

breakLongLines :: T.Text -> T.Text
breakLongLines input = 
  let lines' = map cutLongLine $ T.lines input
  in  go lines'
  where
    go = foldr (\x acc -> x <> "<br/>" <> acc) mempty 

maxlinelength :: Int
maxlinelength = 120

cutLongLine :: T.Text -> T.Text
cutLongLine input 
  | T.length input < maxlinelength = input
  | otherwise = let (f, s) = T.splitAt maxlinelength input
                    nextSpaceIndex = T.findIndex (== ' ') s 
                in case nextSpaceIndex of 
                  Nothing -> f <> s
                  Just pos -> let (s1, s2) = T.splitAt pos s 
                              in  f <> s1 <> "</br>" <> s2

-- breakLongLines :: T.Text -> T.Text
-- breakLongLines input
--   | T.length input < 120 = input
--   | otherwise = let (f, s) = T.splitAt 120 input
--                 in f <> "<br/>" <> s
