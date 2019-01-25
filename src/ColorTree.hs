{-# Language  TypeSynonymInstances #-}
{-# Language  FlexibleInstances #-}

module ColorTree where

import Color

import Data.Ix

import Linear.V3
import Data.Trees.KdTree
import System.Random


type ColorTree = KdTree Color


instance Point Color where
    dimension _ = 3
    
    coord 0 = fromIntegral . getRed
    coord 1 = fromIntegral . getGreen
    coord 2 = fromIntegral . getBlue
    
    dist2 (V3 a b c) (V3 x y z)
        = fromIntegral $ (a - x)^2 + (b - y)^2 + (c - z)^2

    
removeColor :: Color -> ColorTree -> ColorTree
removeColor color colors
    = remove' colors color

    
remove' :: (Eq p, Point p) => KdTree p -> p -> KdTree p
remove' KdEmpty _ = KdEmpty
remove' (KdNode l p r axis) pKill
    | p == pKill
        = fromListWithDepth (toList l ++ toList r) axis
    | coord axis pKill  < coord axis p
        = KdNode (remove' l pKill) p r axis
    | otherwise
        = KdNode l p (remove' r pKill) axis
    

getClosestColor :: Color -> ColorTree -> (Color, ColorTree)
getClosestColor color colorTree
    = (closestColor, removeColor closestColor colorTree)
    where
        Just closestColor = nearestNeighbor colorTree color


makeAllColors :: (Int, Int) -> ColorTree
makeAllColors (x, y)
    = fromList [ toColor i | i <- range bounds ]
    where
        upperBound = head . dropWhile (\z -> z*z*z < x*y) $ [2..]
        bounds = ((0,0,0) , (upperBound-1, upperBound-1, upperBound-1))

        
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