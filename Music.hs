module Music where

import Data.List
import Data.Traversable

-- DURATIONS & RYTHM ======================================================
-- | fractionalDurations
-- > [1, 2, 4, 8, 16, 32, 64]
fractionalDurations :: [Integer]
fractionalDurations = [2^x | x <- take 7 (cycle [0..])]

-- | isLegalDuration
isLegalDuration :: Integer -> Bool
isLegalDuration d = d `elem` fractionalDurations

-- ------------------------------------------------------------------------
-- | TimeSignature
-- > waltz = TimeSignature 3 4
data TimeSignature = TimeSignature
  {
    times         :: Int,
    base_duration :: Int
  }
instance Show TimeSignature where
  show (TimeSignature t b) = (show t) ++ "/" ++ (show b)

-- ------------------------------------------------------------------------
-- | Duration
-- > round = Duration "round" 1
-- data Duration = Duration
--   {
--     duration_name  :: String,
--     duration_value :: Int
--   }
-- instance Show Duration where
--   show (Duration n _) = n

-- NOTES AND SCALES =======================================================
-- | Note
-- > mi = Note "mi" 10 
data Note = Note
  {
    note_name   :: String,
    pitch       :: Int
  }
instance Show Note where
  show (Note n _) = n

allNotes :: ScaleList
allNotes =
  [
    Note "do"   1, Note "dod" 1, Note "reb"  1, Note "re"  1,
    Note "red"  2, Note "mib" 1, Note "mi"   1, Note "mid" 1,
    Note "fa"   1, Note "fad" 1, Note "solb" 1, Note "sol" 1,
    Note "sold" 1, Note "lab" 1, Note "la"   1, Note "lad" 1,
    Note "sib"  1, Note "si"  1, Note "dob"  1
  ]

-- ------------------------------------------------------------------------
type ScaleSchema = [Int]
type ScaleList = [Note]

generateScale scale_schema start_note = do
  show "toto"

-- | Scale
-- each step from note to note
-- ex: Scale "Natural" [0.0, 1.0, 1.0
data Scale = Scale
  {
    scale_start_note :: Note,
    scale_name       :: String,
    scale_list       :: ScaleList
  } --deriving Traversable
-- instance Show Scale where
  -- show (Scale sn n l) = s

-- chromaticSchale :: Scale
-- chromaticSchale = Scale

-- naturalMajorSchema :: ScaleSchema
-- naturalMajorSchema = ScaleSchema [0, 2, 4, 5, 7, 9, 11, 12]  
