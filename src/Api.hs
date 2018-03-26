{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Api where

import           Control.Monad.Trans (liftIO)
import           Data.Monoid
import           Data.Either
import qualified Data.Text as T
import           Data.Proxy
import           Servant.API
import           Servant.Client

import           Network.HTTP.Client (newManager, noProxy, managerSetProxy)
import           Network.HTTP.Client.TLS

import qualified Model.Profile as MP
import qualified Model.RuleInfo as MR


type ProfileApi = "api" :> "profiles" :> 
                    QueryParam "language" T.Text :>
                    QueryParam "name" T.Text :> 
                    Get '[JSON] [MP.Profile]
             :<|> "api" :> "rules" :> "show" :> 
                    QueryParam "key" T.Text :>
                    Get '[JSON] MR.Rule

profileApi :: Proxy ProfileApi
profileApi = Proxy

{-- (a :<|> b) --}
getProfile :: Maybe T.Text -> Maybe T.Text -> ClientM [ MP.Profile ]
getRule :: Maybe T.Text -> ClientM MR.Rule
(getProfile :<|> getRule) = client profileApi

getProfile' :: T.Text -> ClientM [ MP.Profile ]
getProfile' name = 
  getProfile (Just "java") (Just name)

rule :: T.Text -> ClientM MR.Rule
rule key = 
  getRule (Just key)

fetchProfileRules :: String -> IO [MR.RuleDetails]
fetchProfileRules domain = do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  setGlobalManager manager
  res    <- fetchProfiles domain
  
  case res of 
    Left err -> do  
      putStrLn $ "Error: " ++ show err
      return []
    Right profiles -> do 
      let profile = head profiles
          profileRules = MP.rules profile
      putStrLn $ show (length profileRules) ++ " rules found in " ++ showText (MP.name profile)
      rules <- mapM (fetchRule domain) profileRules
--       rules <- mapM (fetchRule domain) $ take 50 profileRules
      printErrors rules 
      return $ map (\(Right r) -> MR.rule r) $ 
        filter isRight rules


fetchProfiles :: String -> IO (Either ServantError [MP.Profile])
fetchProfiles domain = do
  clientEnv <- createClientEnv domain
  runClientM (getProfile' "Sonar way with Findbugs") clientEnv

fetchRule :: String -> MP.ProfileRule -> IO (Either ServantError MR.Rule)
fetchRule domain MP.ProfileRule{..} = do
  clientEnv <- createClientEnv domain
  runClientM (getRule pkey) clientEnv
  where pkey = Just (repo <> ":" <> key)

createClientEnv :: String -> IO ClientEnv
createClientEnv domain = do
  manager <- liftIO getGlobalManager  
  return $ ClientEnv manager (BaseUrl Https domain 443 "")

printErrors :: (Show a, Show b) => [Either a b] -> IO ()
printErrors eithers =
  mapM_ (\err -> putStrLn $ "Error: " ++ show err) $ 
    filter isLeft eithers

showText :: T.Text -> String
showText = T.unpack