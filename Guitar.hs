module Guitar where

import Music
-- ----------------------------------------------------------------------
-- | GuitarString
-- don't get confused between guitar strings and Haskell strings :)
data GuitarString = GuitarString
  {
    empty_string_name :: Note,
    frets             :: Int,
    string_scale      :: Scale
  }

-- -----------------------------------------------------------------------
-- | Guitar Tuning
-- a guitar strings container
type GuitarTuning = [GuitarString]

-- standardTuning :: GuitarTuning
-- standardTuning = [
--   GuitarString Mi 24,
--   GuitarString La 24,
--   GuitarString Re 24,
--   GuitarString Sol 24,
--   GuitarString Si 24,
--   GuitarString Mi 24
--   ]

-- dropDTuning :: GuitarTuning
-- dropDTuning = [GuitarString Re 24] ++ (tail standardTuning)
