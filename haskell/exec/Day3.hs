{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Attoparsec.Text as P
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)
import           Text.Printf          (printf)

import           AOC.Reader           (readInput)

data Entry =
  Entry !Int
        !(Int, Int)
        !(Int, Int)
  deriving (Show)

type EntryMap = HM.HashMap (Int, Int) Int

main :: IO ()
main = do
  elements <- readInput parseLine
  -- Part 1
  let entryMap = buildMap elements
      overlapping = HM.size . HM.filter (> 1) $ entryMap
  putStrLn (printf "Overlapping spaces: %d" overlapping)
  -- Part 2
  let (Entry claimId _ _) = head (filter (checkClaimIntact entryMap) elements)
  putStrLn (printf "Intact claim: #%d" claimId)

buildMap :: [Entry] -> EntryMap
buildMap = HM.fromListWith (+) . concatMap go
  where
    go = fmap (, 1) . pointsFromEntry

checkClaimIntact :: EntryMap -> Entry -> Bool
checkClaimIntact entryMap entry = all isOk points
  where
    points = pointsFromEntry entry
    isOk point = (entryMap HM.! point) == 1

pointsFromEntry :: Entry -> [(Int, Int)]
pointsFromEntry (Entry _ (l, t) (r, b)) =
  [(x, y) | x <- [l .. (l + r - 1)], y <- [t .. (t + b - 1)]]

parseLine :: Text -> Entry
parseLine = fromRight . P.parseOnly entryP
  where
    entryP = do
      claimId <- idP
      wh (P.char '@')
      topLeft <- topLeftP
      wh (P.char ':')
      Entry claimId topLeft <$> sizeP
    wh p = P.skipSpace >> p >> P.skipSpace
    idP = P.char '#' *> P.decimal
    topLeftP = (,) <$> (P.decimal <* P.char ',') <*> P.decimal
    sizeP = (,) <$> (P.decimal <* P.char 'x') <*> P.decimal

fromRight :: Show a => Either a b -> b
fromRight (Right v) = v
fromRight (Left e)  = error ("Either REEE: " ++ show e)
