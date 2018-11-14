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
