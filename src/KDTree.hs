-- Code taken from the kdtree library and modified to be a bit faster for my purposes
-- 
-- 
-- Obligatory copyright notice taken from the kdtree library:
--
-- Copyright (c)2011, 2017, Issac Trotts & Contributors
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
-- 
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
-- 
--     * Neither the name of Issac Trotts nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE  BangPatterns #-}
{-# LANGUAGE  MultiParamTypeClasses #-}
{-# LANGUAGE  FunctionalDependencies #-}

module KDTree where


import qualified Data.Foldable as F
import Data.List
import Data.Maybe


class Point point where
    -- | Returns the number of coordinates of a point.
    dimension :: point -> Int

    -- | Returns the k'th coordinate, starting from 0.
    coord :: Int -> point -> Int

    -- | Returns the squared distance between two points.
    dist2 :: point -> point -> Int
    dist2 a b = sum . map diff2 $ [0 .. dimension a - 1]
        where
            diff2 i = (coord i a - coord i b)^2


-- | Compares the distances of a and b to p.
compareDistance :: (Point p) => p -> p -> p -> Ordering
compareDistance p a b = dist2 p a `compare` dist2 p b


data KdTree point
    = KdNode (KdTree point) point (KdTree point) Int
    | KdEmpty
    deriving (Eq, Ord, Show)

kdPoint :: KdTree p -> p
kdPoint (KdNode _ p _ _) = p

kdLeft, kdRight :: KdTree p -> KdTree p
kdLeft  (KdNode l _ _ _) = l
kdRight (KdNode _ _ r _) = r

kdAxis :: KdTree p -> Int
kdAxis (KdNode _ _ _ a) = a



instance Functor KdTree where
    fmap _ KdEmpty
        = KdEmpty
    fmap f (KdNode l x r axis)
        = KdNode (fmap f l) (f x) (fmap f r) axis


instance Foldable KdTree where
    foldr f init KdEmpty = init
    foldr f init (KdNode l x r _) = F.foldr f init3 l
        where
            init3 = f x init2
            init2 = F.foldr f init r


fromList :: (Point p) => [p] -> KdTree p
fromList points = fromListWithDepth points 0


-- | Selects an axis based on depth so that the axis cycles through all valid values.
fromListWithDepth :: (Point p) => [p] -> Int -> KdTree p
fromListWithDepth [] _ = KdEmpty
fromListWithDepth points depth = node
    where axis = depth `mod` dimension (head points)

          -- Sort point list and choose median as pivot element
          sortedPoints = sortBy (\a b -> coord axis a `compare` coord axis b) points
          medianIndex = length sortedPoints `div` 2
          medianCoordinate = coord axis (sortedPoints !! medianIndex)

          leftPoints = filter (\p -> coord axis p < medianCoordinate) sortedPoints
          trueMedianIndex = length leftPoints
          rightPoints = drop (trueMedianIndex+1) sortedPoints

          -- Create node and construct subtrees
          node = KdNode (fromListWithDepth leftPoints (depth+1)) (sortedPoints !! trueMedianIndex) (fromListWithDepth rightPoints (depth+1)) axis


toList :: KdTree p -> [p]
toList = F.foldr (:) []


-- | Returns the nearest neighbor of p in tree.
nearestNeighbor :: (Point p) => KdTree p -> p -> Maybe p
nearestNeighbor KdEmpty probe = Nothing
nearestNeighbor (KdNode KdEmpty p KdEmpty _) probe = Just p
nearestNeighbor (KdNode l pivot r axis) probe =
    if xProbe < xPivot then findNearest l r else findNearest r l
    where xProbe = coord axis probe
          xPivot = coord axis pivot
          findNearest tree1 tree2 =
                let candidate1 = case nearestNeighbor tree1 probe of
                                   Nothing   -> pivot
                                   Just best -> minimumBy (compareDistance probe) [best, pivot]
                    sphereIntersectsPlane = (xProbe - xPivot)^2 <= dist2 probe candidate1
                    candidates2 = if sphereIntersectsPlane
                                    then candidate1 : maybeToList (nearestNeighbor tree2 probe)
                                    else [candidate1] in
                Just . minimumBy (compareDistance probe) $ candidates2



-- | Removes the point p from t.
remove :: (Eq p, Point p) => KdTree p -> p -> KdTree p
remove KdEmpty _
    = KdEmpty
remove (KdNode l p r axis) pKill
    | p == pKill
        = fromListWithDepth (toList l ++ toList r) axis
    | coord axis pKill < coord axis p
        = KdNode (remove l pKill) p r axis
    | otherwise
        = KdNode l p (remove r pKill) axis