module ColorPicker where

import Color
import ColorPalette

import Data.List

import Linear.V3


-- | `ColorPicker` is a function that takes a chosen color and a container of available colors
-- and returns a color that is closest to that color along with a new container
-- without the picked color
type ColorPicker t a
    = a -> t a -> (a, t a)

-- | `ColorPicker'` is a bit of a hack version of `ColorPicker` for when `t` doesn't take any
-- type arguments.
type ColorPicker' t a
    = a -> t -> (a, t)


-- | Picks the closest unused color to the provided one using ColorPalette.
-- 
-- O(log n)
pickClosest :: ColorPicker' ColorPalette Color
{-# INLINE pickClosest #-}
pickClosest
    = getClosestColor