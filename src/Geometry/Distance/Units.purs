module Geometry.Distance.Units where

foreign import kind SpaceUnit

-- | Some example metrics for reuse and easy interoop
-- | between sublibs

-- | Absolute units in some space
foreign import data Real ∷ SpaceUnit

-- | Physical pixels of an image
foreign import data Pixel ∷ SpaceUnit

-- | Pixels on the screen (from click event etc.)
foreign import data Screen ∷ SpaceUnit
