module Main where

import Control.Monad (replicateM)
import Data.List.Split
import qualified Data.Map as M
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
mapComp :: KVPairs -> IO ()
mapComp kvpairs = do
  let init = M.empty
  let m = foldr ins init kvpairs where
        ins (k, v) t = M.insert k v t
  if M.size m /= length kvpairs
  then putStrLn $ "FAIL: " ++ show (M.size m) ++ ", " ++ show (length kvpairs)
  else pure $ ()


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
  timing $ mapComp kvp1

  kvp2 <- makeKVPairs 10000
  timing $ mapComp kvp2

  kvp3 <- makeKVPairs 100000
  timing $ mapComp kvp3
  

main :: IO ()
main = do
  timing $ simpleComp

  _ <- testWrites

  _ <- testMap

  pure ()


--------------------------------------------------
-- Sample run
----------------------------------------

-- > $ stack exec -- state-play-exe                                                                   
-- Hello World
-- Time: 0.326 msec.
-- Time: 0.368 msec.
-- Time: 0.477 msec.
-- Time: 32.499 msec.
-- Time: 204.347 msec.
-- Time: 472.092 msec.
-- Time: 3.744 msec.
-- Time: 346.244 msec.
-- Time: 4130.675 msec.
