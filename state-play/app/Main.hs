module Main where

import Control.Monad (replicateM)
import Data.List.Split
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HMS
import System.CPUTime
import System.IO
import System.Random

-- TODO: define a record for computations ...

timing :: IO a -> IO a
timing comp = do
  start <- getCPUTime
  v <- comp  -- Perform the actual computation
  end <- getCPUTime
  let diff = end - start  -- fyi: picoseconds
  let diff_millies = (fromIntegral diff) / (10 ^ 9)
  putStrLn $ "Time: " ++ (show diff_millies) ++ " msec."
  pure v

-- Simplest possible thing, as a baseline
simpleComp :: IO ()
simpleComp = do
  putStrLn "Hello World"

-- Open a file in /tmp, write a large string
writeComp :: String -> IO ()
writeComp s = do
  writeFile "/tmp/foobar.txt" s
  pure ()


-- Simple, non-randomized string
makeLongString :: Int -> String
makeLongString count =
  take count $ cycle ['a' .. 'z']


-- A few helpers to create random strings and lists of string/int pairs
keylen = 30

type KVPairs = [(String, Int)]
type CharStream = [Char]

getRandInts count g = take count $ randoms g :: [Int]

getRandStrings :: RandomGen stdGen => Int -> stdGen -> [String]
getRandStrings count g = take count $ chunksOf keylen $ randomRs ('A', 'z') g

makeKVPairs :: Int -> IO KVPairs
makeKVPairs count = do
  g <- getStdGen
  let randints = getRandInts count g
  let randstrs = getRandStrings count g
  pure $ zip randstrs randints


-- Basic map insertion
data MapOps m = MapOps
  {
    empty :: m
  , insert :: String -> Int -> m -> m
  , size :: m -> Int
  }


plainOps = MapOps M.empty M.insert M.size
hashmapLazyOps = MapOps HML.empty HML.insert HML.size
hashmapStrictOps = MapOps HMS.empty HMS.insert HML.size

mapComp :: KVPairs -> MapOps m -> IO ()
mapComp kvpairs mapops = do
  let init = empty mapops
  let m = foldr ins init kvpairs where
        ins (k, v) t = (insert mapops) k v t
  if (size mapops) m /= length kvpairs
  then putStrLn $ "FAIL: " ++ show ((size mapops) m) ++ ", " ++ show (length kvpairs)
  else pure ()


testWrites = do
  let str1 = makeLongString 5000
  timing $ writeComp str1

  let str2 = makeLongString 500000
  timing $ writeComp str2

  let str3 = makeLongString 5000000
  timing $ writeComp str3

  let str4 = makeLongString 10000000
  timing $ writeComp str4


testMap = do
  kvp1 <- makeKVPairs 100
  timing $ mapComp kvp1 plainOps

  kvp2 <- makeKVPairs 10000
  timing $ mapComp kvp2 plainOps

  kvp3 <- makeKVPairs 100000
  timing $ mapComp kvp3 plainOps
  

testHashMap isStrict = do
  let mapops = if isStrict then hashmapStrictOps else hashmapLazyOps

  kvp1 <- makeKVPairs 100
  timing $ mapComp kvp1 mapops

  kvp2 <- makeKVPairs 10000
  timing $ mapComp kvp2 mapops

  kvp3 <- makeKVPairs 100000
  timing $ mapComp kvp3 mapops


main :: IO ()
main = do
  timing $ simpleComp

  putStrLn "\nFile write tests"
  testWrites

  putStrLn "\nMap write tests"
  testMap

  putStrLn "\nHashmap (lazy) write tests"
  testHashMap False

  putStrLn "\nHashmap (strict) write tests"
  testHashMap True

  pure ()


--------------------------------------------------
-- Sample run
----------------------------------------

-- > $ stack exec -- state-play-exe                                                                   
-- Hello World
-- Time: 0.312 msec.

-- File write tests
-- Time: 1.399 msec.
-- Time: 43.844 msec.
-- Time: 198.542 msec.
-- Time: 394.335 msec.

-- Map write tests
-- Time: 3.06 msec.
-- Time: 339.551 msec.
-- Time: 3898.309 msec.

-- Hashmap (lazy) write tests
-- Time: 3.997 msec.
-- Time: 442.385 msec.
-- Time: 5325.372 msec.

-- Hashmap (strict) write tests
-- Time: 14.583 msec.
-- Time: 450.123 msec.
-- Time: 5986.775 msec.
