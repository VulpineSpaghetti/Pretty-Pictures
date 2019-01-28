{-# Language  TypeSynonymInstances #-}
{-# Language  FlexibleInstances #-}
{-# Language  MultiParamTypeClasses #-}

module ColorPalette where

import Color
import KDTree

import Data.Ix

import Linear.V3
import System.Random


-- | Represents the remaining colors to paint the picture with
type ColorPalette = KdTree Color


-- Necessary typeclass instance so that it can be used in KdTree
instance Point Color where
    dimension _ = 3
    
    coord 0 = getRed
    coord 1 = getGreen
    coord 2 = getBlue
    
    dist2 (V3 a b c) (V3 x y z)
        = (a - x)^2 + (b - y)^2 + (c - z)^2


-- | Removes color from the color palette 
removeColor :: Color -> ColorPalette -> ColorPalette
removeColor color colors
    = remove colors color


-- | Returns the closest unused color in the color palette to the given one.
getClosestColor :: Color -> ColorPalette -> (Color, ColorPalette)
getClosestColor color colors
    = (closestColor, removeColor closestColor colors)
    where
        Just closestColor = nearestNeighbor colors color


-- | Makes a color palette with all colors, so that there are more colors than pixels.
-- 
-- It takes the second argument to be the resolution of the resulting image, from which it
-- figures out how many pixels will be in the image.
makeAllColors :: (Int, Int) -> ColorPalette
makeAllColors (x, y)
    = fromList [ toColor i | i <- range bounds ]
    where
        upperBound = head . dropWhile (\z -> z*z*z < x*y) $ [2..]
        bounds = ((0,0,0) , (upperBound-1, upperBound-1, upperBound-1))


-- | Randomly picks few random colors from the color palette and returns them.
pickStartingColors :: RandomGen gen => gen -> ColorPalette -> Int -> ([Color], ColorPalette)
pickStartingColors _ colors 0
    = ([], colors)
pickStartingColors randomGenerator colors n
    = (nearestColor : listColors, finalColorPalette)
    where
        (V3 maxX maxY maxZ) = maximum . toList $ colors
        (randX, newRandGen) = randomR (0, maxX) randomGenerator
        (randY, newRandGen') = randomR (0, maxY) newRandGen
        (randZ, newRandGen'') = randomR (0, maxZ) newRandGen'
        
        (nearestColor, newColors) = getClosestColor (V3 randX randY randZ) colors
        
        (listColors, finalColorPalette) =  pickStartingColors newRandGen'' newColors (n-1)