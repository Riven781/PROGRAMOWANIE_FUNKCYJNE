{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)


data AppendData = AppendData
  { listD    :: [Int]
  , index    :: Int
  , newValue :: Int
  } deriving (Show, Generic)

instance FromJSON AppendData

append :: [Int] -> Int -> Int -> [Int]
append listOrg idx newVal = take idx listOrg ++ [newVal] ++ drop idx listOrg


data Pow2Data = Pow2Data
  { list_1 :: [Int]
  , list_2 :: [Int]
  } deriving (Show, Generic)

instance FromJSON Pow2Data

pow2 :: [Int] -> [Int] -> [Int]
pow2 xs ys = concatMap (\(x, y) -> [x*x, y*y]) (zip xs ys)


data ConcatenationData = ConcatenationData
  { list_1__ :: [Int]
  , list_2__ :: [Int]
  , list_3__ :: [Int]
  } deriving (Show, Generic)

instance FromJSON ConcatenationData

concatenate :: [Int] -> [Int] -> [Int] -> [Int]
concatenate a b c = a <> b <> c


data ListResponse = ListResponse
  { resultList :: [Int]
  } deriving (Show, Generic)

instance ToJSON ListResponse


main :: IO ()
main = scotty 8080 $ do
  get "/" $ text "Haskell (Scotty) app running in Docker. Endpoints: /append /pow2 /concatenate"

  post "/append" $ do
    d <- jsonData :: ActionM AppendData
    let out = append (listD d) (index d) (newValue d)
    json $ ListResponse out


  post "/pow2" $ do
    d <- jsonData :: ActionM Pow2Data
    let out = pow2 (list_1 d) (list_2 d)
    json $ ListResponse out


  post "/concatenate" $ do
    d <- jsonData :: ActionM ConcatenationData
    let out = concatenate (list_1__ d) (list_2__ d) (list_3__ d)
    json $ ListResponse out
