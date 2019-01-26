module ColorPicker where

import Color
import ColorTree

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


-- | Picks the closest color to the provided one using a simple fold.
-- 
-- O(n)
pickClosestFold :: (Ord n, Integral n) => ColorPicker [] (V3 n)
{-# INLINE pickClosestFold #-}
pickClosestFold targetColor colors
    = (pickedColor, delete pickedColor colors)
    where
        pickedColor = foldr1 accumFun colors
        
        accumFun newColor oldColor
            | colorDist targetColor newColor < colorDist targetColor oldColor
                = newColor
            | otherwise
                = oldColor


-- | Picks the closest color to the provided one using the ColorTree.
-- 
-- O(log n)
pickClosestCT :: ColorPicker' ColorTree Color
{-# INLINE pickClosestCT #-}
pickClosestCT
    = ColorTree.getClosestColor