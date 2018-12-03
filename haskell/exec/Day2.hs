module Main where

import qualified Data.HashMap.Strict as HM
import           Data.List           (foldl', tails)
import           Data.Maybe          (catMaybes, isJust)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Read      (decimal, signed)
import           Text.Printf         (printf)

import           AOC.Reader          (readInput)

main :: IO ()
main = do
  elements <- readInput id
  -- Part 1
  let twoChar = length $ filter (hasNRepeatedChars 2) elements
      threeChar = length $ filter (hasNRepeatedChars 3) elements
      checksum = twoChar * threeChar
  putStrLn $ printf "Checksum: %d" checksum

  -- Part 2
  let (commonBoxPath:_) =
        [ commonPath
        | (first:rest) <- tails elements
        , candidate <- rest
        , Just commonPath <- [oneCharDifference first candidate]
        ]
  putStrLn $ printf "Common path: %s" commonBoxPath

freqMap :: Text -> HM.HashMap Char Int
freqMap = HM.fromListWith (+) . flip zip (repeat 1) . Text.unpack

hasNRepeatedChars :: Int -> Text -> Bool
hasNRepeatedChars n = elem n . HM.elems . freqMap

oneCharDifference :: Text -> Text -> Maybe String
oneCharDifference xs ys =
  if valid
    then Just answer
    else Nothing
  where
    compared =
      (\(x, y) ->
         if x == y
           then Just x
           else Nothing) <$>
      Text.zip xs ys
    valid = (== strLen - 1) . length . filter isJust $ compared
    answer = catMaybes compared
    strLen = Text.length xs
