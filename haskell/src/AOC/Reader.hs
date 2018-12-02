module AOC.Reader
  ( readInput
  ) where

import           Prelude      hiding (getContents, lines)

import           Data.Text    (Text, lines)
import           Data.Text.IO (getContents)

readInput :: (Text -> a) -> IO [a]
readInput f = fmap f . lines <$> getContents
