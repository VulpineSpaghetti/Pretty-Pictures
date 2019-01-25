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

type ColorPicker' t a
    = a -> t -> (a, t)


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


pickClosestCT :: ColorPicker' ColorTree Color
{-# INLINE pickClosestCT #-}
pickClosestCT
    = ColorTree.getClosestColor