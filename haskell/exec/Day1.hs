module Main where

import           Data.List      (foldl', scanl')
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text      as Text
import           Data.Text.Read (decimal, signed)
import           Text.Printf    (printf)

import           AOC.Reader     (readInput)

main :: IO ()
main = do
  elements <- readInput readInputLine
  -- Part 1
  let freq = foldl' (+) 0 elements
  putStrLn (printf "Frequency: %d" freq)
  -- Part 2
  let infiniteElements = elements ++ infiniteElements
      answerArray = scanl' (+) 0 infiniteElements
      dupFreq = firstDupe answerArray
  putStrLn (printf "First duplicate frequency: %d" dupFreq)

readInputLine :: Text -> Int
readInputLine i = result
  where
    Right (result, _) = parsedLine
    parsedLine = signed decimal i

firstDupe :: [Int] -> Int
firstDupe xs = dupe xs Set.empty
  where
    dupe (x:xs) s =
      if Set.member x s
        then x
        else dupe xs (Set.insert x s)
