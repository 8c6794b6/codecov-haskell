-- |
-- Module:      Trace.Hpc.Codecov.Lix
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
-- Portability: portable
--
-- Functions for converting hpc output to line-based code coverage data.

module Trace.Hpc.Codecov.Lix where

-- base
import Data.List              (sortBy)
import Data.Ord               (comparing)
import Prelude                hiding (getLine)

-- hpc
import Trace.Hpc.Mix          (BoxLabel (..), CondBox (..), MixEntry)
import Trace.Hpc.Util         (fromHpcPos)

-- Internal
import Trace.Hpc.Codecov.Util

type CoverageEntry = (
    [MixEntry], -- mix entries
    [Integer],  -- tix values
    [String])   -- entry source code

data Hit = Full
         | Partial
         | None
         | Irrelevant
    deriving (Eq, Show)

type Lix = [Hit]

toHit :: [Bool] -> Hit
toHit []  = Irrelevant
toHit [x] = if x then Full else None
toHit xs
    | and xs    = Full
    | or xs     = Partial
    | otherwise = None

getLine :: MixEntry -> Int
getLine = fffst . fromHpcPos . fst
    where fffst (x, _, _, _) = x

toLineHit :: CoverageEntry -> (Int, Bool)
toLineHit (entries, counts, _source) = (getLine (head entries) - 1, all (> 0) counts)

isOtherwiseEntry :: CoverageEntry -> Bool
isOtherwiseEntry (mixEntries, _, source) =
    source == ["otherwise"] && boxLabels == otherwiseBoxLabels
    where boxLabels = map snd mixEntries
          otherwiseBoxLabels = [
              ExpBox False,
              BinBox GuardBinBox True,
              BinBox GuardBinBox False]

adjust :: CoverageEntry -> CoverageEntry
adjust coverageEntry@(mixEntries, tixs, source) =
    if isOtherwiseEntry coverageEntry && any (> 0) tixs
    then (mixEntries, [1, 1, 1], source)
    else coverageEntry

-- | Convert hpc coverage entries into a line based coverage format
toLix :: Int             -- ^ Source line count
      -> [CoverageEntry] -- ^ Mix entries and associated hit count
      -> Lix             -- ^ Line coverage
toLix lineCount entries = map toHit (groupByIndex lineCount sortedLineHits)
    where sortedLineHits = sortBy (comparing fst) lineHits
          lineHits = map (toLineHit . adjust) entries
