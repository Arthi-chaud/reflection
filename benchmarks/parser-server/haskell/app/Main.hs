{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.DeepSeq
import qualified Data.Aeson as Aeson
import Data.ByteString (toStrict)
import Data.JSON.FromJSON
import Data.JSON.TH (genFromJSON)
import FlatParse.Basic
import GHC.Generics (Generic)
import System.CPUTime
import System.Environment
import Web.Scotty

data DTO = DTO {name :: String, boolean :: Bool} deriving (Generic)

newtype Response = Response {computationTime :: Float} deriving (Generic)

instance NFData DTO
instance Aeson.FromJSON DTO
instance Aeson.ToJSON Response

genFromJSON ''DTO

main :: IO ()
main = do
    port <- read <$> getEnv "PORT"
    scotty port $ do
        get "/" $ file "./index.html"
        post "/aeson" $ do
            bodyBS <- toStrict <$> body
            diff <- computeTime $ maybe (fail "invalid dto") (return . rnf) (Aeson.decodeStrict @[DTO] bodyBS)
            json $ Response diff

        post "/reflection" $ do
            bodyBS <- toStrict <$> body
            diff <-
                computeTime $
                    rnf <$> case runParser (parse @[DTO]) bodyBS of
                        OK obj _ -> return obj
                        _ -> fail "invalid dto"
            json $ Response diff
  where
    {-# INLINE computeTime #-}
    computeTime op = do
        !start <- liftIO getCPUTime
        () <- op
        !end <- liftIO getCPUTime
        return $ fromIntegral (end - start) / (10 ^ (12 :: Integer))
