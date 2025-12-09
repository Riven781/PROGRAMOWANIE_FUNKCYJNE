{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Web.Scotty
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import System.Random (StdGen, randomR, getStdGen)
import Control.Concurrent (newMVar, putMVar, takeMVar)



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

data AppendData = AppendData { listD :: [Int],  index :: Int, newValue :: Int} deriving (Show, Generic)
instance FromJSON AppendData

append :: [Int] -> Int -> Int -> [Int]
append listOrg idx newVal =  take idx listOrg ++ [newVal] ++ drop idx listOrg

data Pow2Data = Pow2Data {list_1 :: [Int], list_2 :: [Int] } deriving (Show, Generic)
instance FromJSON Pow2Data

pow2 :: [Int] -> [Int] -> [Int]
pow2 list__1 list__2 = concatMap (\(x, y) -> [x*x, y*y] ) $ zip list__1 list__2


randInt :: StdGen -> (Int, StdGen)
randInt gen = randomR (1, maxBound::Int) gen

data RandResponse = RandResponse { intVal :: Int} deriving (Show, Generic)
instance ToJSON RandResponse

randDouble :: StdGen -> (Double, StdGen)
randDouble gen = 
  let (doubleVal__, gen2) = randomR (0.0, 1.0) gen
  in if doubleVal__ == 1.0 then randDouble gen2 else (doubleVal__, gen2)

data RandDoubleResponse = RandDoubleResponse { doubleVal :: Double} deriving (Show, Generic)
instance ToJSON RandDoubleResponse


main :: IO ()
main = do 
  gen <- getStdGen
  genVar <- newMVar gen
  scotty 8080 $ do
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

    --zad 4.5
    post "/append" $ do
      appendData <- jsonData :: ActionM AppendData
      let listD_ = listD appendData
      let index_ = index appendData
      let newValue_ = newValue appendData
      let resultAppend = append listD_ index_ newValue_
      json $ SumResponse resultAppend

    --zad 5.0
    post "/pow2" $ do
      pow2Data <- jsonData :: ActionM Pow2Data
      let list_1_ = list_1 pow2Data
      let list_2_ = list_2 pow2Data
      let resultPow2 = pow2 list_1_ list_2_
      json $ SumResponse resultPow2


    --ZADANIE 7

    --zad3.0
    get "/randInt" $ do
      genOld <- liftIO $ takeMVar genVar
      let (intVal_, gen_) = randInt genOld
      liftIO  $ putMVar genVar gen_
      json $ RandResponse intVal_ 

    --zad3.5
    get "/randDouble" $ do
      genOld <- liftIO $ takeMVar genVar
      let (doubleVal_, gen_) = randDouble genOld
      liftIO  $ putMVar genVar gen_
      json $ RandDoubleResponse doubleVal_ 
      



