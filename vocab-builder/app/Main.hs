{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

-- Layout of functions we're going to define

type Entry = (T.Text, Int)     -- one entry
type Vocabulary = [Entry]      -- a list of entries

extractVocab :: T.Text -> Vocabulary
extractVocab text  = map buildEntry $ group $ sort ws
    where
      ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWords $ T.words text
      cleanWords = T.dropAround $ not . isLetter
      buildEntry ws@(w:_) = (w, length ws)


wordsCount :: Vocabulary -> Int
wordsCount vocab = sum $ map snd vocab


wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)


allWordsReport :: Vocabulary -> T.Text
allWordsReport vocab
  = T.append "\nAll words:\n"
  $ T.unlines
  $ map fst vocab


wordsCountReport :: Vocabulary -> T.Text
wordsCountReport vocab
  = T.append "\nTotal number of words:\n"
  $ T.pack
  $ show
  $ wordsCount vocab


frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab n
  = T.append "\nFrequent words:\n"
  $ T.unlines
  $ map showEntry
  $ take n
  $ wordsByFrequency vocab
  where
    showEntry (t, n)
      = T.append t
      $ T.pack
      $ " - " ++ show n
                       

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "Showing all words : "
  TIO.putStrLn $ T.unlines $ map fst vocab


processTextFile :: FilePath -> Int -> IO ()
processTextFile fname n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n


main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname, n] -> processTextFile fname (read n)
    _ -> putStrLn "Usage: vocab-builder file-name"

     

