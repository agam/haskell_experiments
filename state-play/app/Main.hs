module Main where

--import Data.HashMap
import System.CPUTime
import System.IO

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

-- TODO
-- Hash map insertion
  

main :: IO ()
main = do
  timing $ simpleComp

  timing $ writeComp "something"

  let str1 = makeLongString 5000
  timing $ writeComp str1

  let str2 = makeLongString 500000
  timing $ writeComp str2

  let str3 = makeLongString 50000000
  timing $ writeComp str3


--------------------------------------------------
-- Sample run
----------------------------------------

-- > $ stack exec -- state-play-exe                                                                   
-- Hello World
-- Time: 0.309 msec.
-- Time: 0.485 msec.
-- Time: 0.585 msec.
-- Time: 40.243 msec.
-- Time: 1907.859 msec.
