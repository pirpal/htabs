module Music where

import System.IO
import Data.Char
import Data.List
import Data.Traversable

-- GENERAL ================================================================

data Language = FR | EN deriving (Eq, Show)

-- DURATIONS & RYTHM ======================================================

intDurations :: [Int]
intDurations = [1, 2, 4, 8, 16, 32, 64]

fractionalDuration :: Fractional a => a -> a
fractionalDuration d = 1/d

-- |> waltz = TimeSignature 3 4
data TimeSignature = TimeSignature
  {
    times    :: Int,
    duration :: Int
  }
instance Show TimeSignature where
  show (TimeSignature t d) = (show t) ++ "/" ++ (show d)


-- NOTES & SCALES =========================================================
type Scale = [Note]
type ScalePattern = [Int]
type Chord = [Note]
type Armure = [Note] -- ?
type NoteName = (NoteNameFR, NoteNameEN)

data NoteNameFR = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Ord, Show)
data NoteNameEN = C  | D  | E  | F  | G   | A  | B  deriving (Eq, Ord, Show)

notesNames :: [NoteName]
notesNames = zip [Do, Re, Mi, Fa, Sol, La, Si] [C, D, E, F, G, A, B]

notesNamesStartingFrom :: NoteName  -> [NoteName]
notesNamesStartingFrom nn = (
  dropWhile (/= nn) notesNames ++ takeWhile (/= nn) notesNames)

getNoteName :: Int -> NoteName
getNoteName n
  | n `elem` [0..6] = head (drop n notesNames)
  | otherwise = head notesNames -- (Do, C) by default

naturalMajorPattern :: ScalePattern
naturalMajorPattern = [0, 2, 4, 5, 7, 9, 11, 12] -- 1 == 1/2 tone

data Alteration =  Natural | Flat | Sharp deriving (Eq, Ord)
instance Show Alteration where
  show Natural = ""
  show Flat  = "b" -- Bemol
  show Sharp = "#" -- Diese

data Pointed = Empty | Point deriving (Eq, Ord)
instance Show Pointed where
  show Empty = ""
  show Point = "."

data Note = Note
  {
    _note_index      :: Int,
    _note_names      :: (NoteNameFR, NoteNameEN),
    _note_alteration :: Alteration,
    _note_duration   :: Int,
    _note_pointed    :: Pointed
  } deriving (Eq, Ord, Show)

nameFR :: Note -> String
nameFR (Note _ n a d p) = show (fst n) ++ show a ++ show d ++ show p

nameEN :: Note -> String
nameEN (Note _ n a d p) = show (snd n) ++ show a ++ show d ++ show p

lowerName :: String -> String
lowerName n = [(toLower (head n))] ++ (tail n)

lngName :: Note -> Language -> String
lngName n l
  | l == FR = lowerName (nameFR n)
  | l == EN = lowerName (nameEN n)
  | otherwise = "LANGUAGE ERROR"

getIndex :: Note -> Int
getIndex (Note i _ _ _ _ ) = i

getDuration :: Note -> Int
getDuration (Note _ _ _ d _) = d 

miMajorScale :: Scale
miMajorScale = [
  Note 0  (Mi,  E) Natural 4 Empty,
  Note 2  (Fa,  F) Sharp   4 Empty,
  Note 4  (Sol, G) Sharp   4 Empty,
  Note 5  (La,  A) Natural 4 Empty,
  Note 7  (Si,  B) Natural 4 Empty,
  Note 9  (Do,  C) Sharp   4 Empty,
  Note 11 (Re,  D) Sharp   4 Empty]

-- naturalNotes :: []
naturalNotes = do
  forM naturalMajorPattern $ \i -> do
    show (i)

-- do dod re mib mi fa fad sol sold la sib si
-- octave :: Note -> Note
-- octave n = Note ((getIndex n) + 1) (nameFR n) (nameEN n) - - -)
-- initNoteFromTuple :: (Int, String) -> Note
-- initNoteFromTuple t = Note (snd t) (fst t)

-- chromaticScaleFrom :: Monad m => [Char] -> Int -> m Scale
-- chromaticScaleFrom note nb = do
--   let t = zip [0..] (take nb (notesNamesStartingFrom note))
--   return (take nb (cycle (map (initNoteFromTuple) t)))
  
-- patternScaleFrom note patt = do
--   let lst = []
--   let cs = head (chromaticScaleFrom note 12)
  -- if length cs == 1 then putStrLn "1 " else putStrLn "+.."
  -- forM patt $ \index -> do
    -- putStrLn note
    -- lst ++ [cs !! index]
  -- lst ++ [(head (head cs))]
  -- return lst
    
-- HARMONY ==============================================================

data Degrees = I | II | III | IV | V | VI | VII deriving (Ord, Eq, Show)
         
-- STAFF ================================================================

data Staff = Staff
  {
    _staff_num       :: Int,
    _staff_time_sign :: TimeSignature,
    _staff_scale     :: Scale, -- Armure
    _staff_notes     :: [Note]
  }

staffTotalTime :: Staff -> Int
staffTotalTime (Staff _ _ _ n) =  sum (map (getDuration) n)
