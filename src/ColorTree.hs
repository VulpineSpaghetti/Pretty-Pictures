{-# Language  TypeSynonymInstances #-}
{-# Language  FlexibleInstances #-}

module ColorTree where

import Color

import Data.Ix

import Linear.V3
import Data.Trees.KdTree
import System.Random


-- | Represents the remaining colors to paint the picture with
type ColorTree = KdTree Color


-- Necessary typeclass instance so that it can be used in KdTree
instance Point Color where
    dimension _ = 3
    
    coord 0 = fromIntegral . getRed
    coord 1 = fromIntegral . getGreen
    coord 2 = fromIntegral . getBlue
    
    dist2 (V3 a b c) (V3 x y z)
        = fromIntegral $ (a - x)^2 + (b - y)^2 + (c - z)^2


-- | Removes color from the color palette 
removeColor :: Color -> ColorTree -> ColorTree
removeColor color colors
    = remove' colors color


-- My edit of the internal `remove` function from KdTree. The one in the library has a major bug,
-- so I had to copy it over here and fix it.
remove' :: (Eq p, Point p) => KdTree p -> p -> KdTree p
remove' KdEmpty _ = KdEmpty
remove' (KdNode l p r axis) pKill
    | p == pKill
        = fromListWithDepth (toList l ++ toList r) axis
    | coord axis pKill  < coord axis p
        = KdNode (remove' l pKill) p r axis
    | otherwise
        = KdNode l p (remove' r pKill) axis


-- | Returns the closest unused color in the color palette to the given one.
getClosestColor :: Color -> ColorTree -> (Color, ColorTree)
getClosestColor color colorTree
    = (closestColor, removeColor closestColor colorTree)
    where
        Just closestColor = nearestNeighbor colorTree color


-- | Makes a color palette with all colors, so that there are more colors than pixels.
-- 
-- It takes the second argument to be the resolution of the resulting image, from which it
-- figures out how many pixels will be in the image.
makeAllColors :: (Int, Int) -> ColorTree
makeAllColors (x, y)
    = fromList [ toColor i | i <- range bounds ]
    where
        upperBound = head . dropWhile (\z -> z*z*z < x*y) $ [2..]
        bounds = ((0,0,0) , (upperBound-1, upperBound-1, upperBound-1))


-- | Randomly picks few random colors from the color palette and returns them.
pickStartingColors :: RandomGen gen => gen -> ColorTree -> Int -> ([Color], ColorTree)
pickStartingColors _ colors 0
    = ([], colors)
pickStartingColors randomGenerator colors n
    = (nearestColor : listColors, finalColorTree)
    where
        (V3 maxX maxY maxZ) = maximum . toList $ colors
        (randX, newRandGen) = randomR (0, maxX) randomGenerator
        (randY, newRandGen') = randomR (0, maxY) newRandGen
        (randZ, newRandGen'') = randomR (0, maxZ) newRandGen'
        
        (nearestColor, newColors) = getClosestColor (V3 randX randY randZ) colors
        
        (listColors, finalColorTree) =  pickStartingColors newRandGen'' newColors (n-1)