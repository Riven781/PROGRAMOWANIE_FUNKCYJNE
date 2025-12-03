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

data SumData = SumData { list1 :: [Int], list2 :: [Int], list3 :: [Int] } deriving (Show, Generic)
instance FromJSON SumData

sum3 :: Int -> Int -> Int -> Int
sum3 a b c = a + b + c

from3to2 :: (Int -> Int -> Int -> Int) -> Int -> (Int -> Int -> Int)
from3to2 f3 value = \a b -> f3 a b value

sumLists :: [Int] -> [Int] -> [Int] -> [Int]
sumLists (list1_h:list1_t) (list2_h:list2_t) (list3_h:list3_t) = let 
  f3 = sum3
  f2 = from3to2 f3 list3_h
  in f2 list1_h list2_h : sumLists (list1_t) (list2_t) (list3_t)
sumLists _ _ _ = []


data SumResponse = SumResponse { resultList :: [Int] } deriving (Show, Generic)
instance ToJSON SumResponse

data SetHeadData = SetHeadData { listData :: [Int], newHeadValue :: Int } deriving (Show, Generic)
instance FromJSON SetHeadData

setHead :: [a] -> a -> [a]
setHead listData_ newHeadValue_ = newHeadValue_ : listData_



main :: IO ()
main = scotty 8080 $ do
  get "/" $ text "Hello from Scotty inside Docker!"

  --zad 3.0
  post "/isSorted" $ do
    isSortedData <- jsonData :: ActionM IsSortedData
    let list_ = list isSortedData
    let f_str = fun isSortedData
    let f = chooseFunction f_str

    let resultSorted  = isSorted list_ f
    json $ IsSortedResponse resultSorted 

  --zad 3.5
  post "/sum" $ do
    sumData <- jsonData :: ActionM SumData
    let list1_ = list1 sumData
    let list2_ = list2 sumData
    let list3_ = list3 sumData
    let resultSum = sumLists list1_ list2_ list3_
    json $ SumResponse resultSum

  --zad 4.0
  post "/setHead" $ do
    setHeadData <- jsonData :: ActionM SetHeadData
    let list_ = listData setHeadData
    let value_ = newHeadValue setHeadData
    let resultSetHead = setHead list_ value_
    json $ SumResponse resultSetHead

