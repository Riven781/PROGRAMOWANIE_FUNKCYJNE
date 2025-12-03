{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Web.Scotty
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)


data IsSortedData = IsSortedData { list :: [Int], fun :: String } deriving (Show, Generic)
instance FromJSON IsSortedData
isSorted :: [Int] -> (Int -> Int -> Bool) -> Bool
isSorted [] _ = True
isSorted[_] _ = True
isSorted (x : y : xs) f = f x y && isSorted (y : xs) f


chooseFunction :: String -> (Int -> Int -> Bool)
chooseFunction "asc" = \x y -> x <= y
chooseFunction "desc" = \x y -> x >= y
chooseFunction "absAsc" = (\x y -> (abs x) <= (abs y))
chooseFunction "absDesc" = (\x y -> (abs x) >= (abs y))
chooseFunction _ = \x y -> x <= y

data IsSortedResponse = IsSortedResponse { result :: Bool } deriving (Show, Generic)
instance ToJSON IsSortedResponse


main :: IO ()
main = scotty 8080 $ do
  get "/" $ text "Hello from Scotty inside Docker!"


  post "/isSorted" $ do
    isSortedData <- jsonData :: ActionM IsSortedData
    let list_ = list isSortedData
    let f_str = fun isSortedData
    let f = chooseFunction f_str

    let resultSorted  = isSorted list_ f
    json $ IsSortedResponse resultSorted 
