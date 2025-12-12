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


data RandPairsResponse = RandPairsResponse { pair1 :: (Int, Double), pair2 :: (Double, Int), tuple3 :: (Double, Double, Double) } deriving (Show, Generic)
instance ToJSON RandPairsResponse

randDoubleIntPair :: StdGen -> ((Double, Int), StdGen)
randDoubleIntPair gen =
  let (doubleVal, gen2) = randDouble gen
      (intVal, gen3) = randInt gen2
  in ((doubleVal, intVal), gen3)

randIntDoublePair :: StdGen -> ((Int, Double), StdGen)
randIntDoublePair gen =
  let (intVal, gen2) = randInt gen
      (doubleVal, gen3) = randDouble gen2
  in ((intVal, doubleVal), gen3)

randTuple :: StdGen -> ((Double, Double, Double), StdGen)
randTuple gen =
  let (doubleVal1, gen2) = randDouble gen
      (doubleVal2, gen3) = randDouble gen2
      (doubleVal3, gen4) = randDouble gen3
  in ((doubleVal1, doubleVal2, doubleVal3), gen4)

data FmapData = FmapData { values :: Maybe (Int, Int), op :: String } deriving (Show, Generic)
instance FromJSON FmapData

addPair:: (Int, Int) -> Int
addPair (x, y) = (x + y)

subtractPair:: (Int, Int) -> Int
subtractPair (x, y) = (x - y)

addOrSubtract :: Maybe (Int, Int) -> String -> Maybe Int
addOrSubtract pair operation  = if operation == "add" then fmap addPair pair else fmap subtractPair pair

data FmapResponse = FmapResponse { operationResult :: Maybe Int } deriving (Show, Generic)
instance ToJSON FmapResponse


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

    --zad4.0
    get "/randPairs" $ do
      genOld <- liftIO $ takeMVar genVar
      let (doubleIntPair, gen_) = randDoubleIntPair genOld
      let (intDoublePair, gen_2) = randIntDoublePair gen_
      let (tuple3, gen_3) = randTuple gen_2
      liftIO  $ putMVar genVar gen_3
      json $ RandPairsResponse intDoublePair doubleIntPair tuple3

  --ZADANIE 8

    --zad3.0
    post "/fmapTask" $ do
      fmapData <- jsonData :: ActionM FmapData
      let pair_ = values fmapData
      let operation_ = op fmapData
      let result = addOrSubtract  pair_ operation_
      json $ FmapResponse result

