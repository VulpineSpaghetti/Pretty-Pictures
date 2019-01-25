module IndexPath
(   IndexPath

,   trivialPath
,   mathPath

,   diamondPath
,   diamondPath2
,   invertedDiamondPath
,   circularPath
,   roundedSquarePath
,   axisPath
,   wavesPath
,   linePath
,   saddleWavePath
,   sinCosPath
,   halfPlane

,   squareSpiralPath
) where

import Prelude hiding (Left,Right)

import Data.List
import Data.Bifunctor


-- | `IndexPath` is a function that gets the starting index and a container with indices
-- and returns a new container with indices sorted in a particular order
type IndexPath t a = (a -> t a -> t a)


-- | Does nothing
trivialPath :: IndexPath t a
trivialPath _ = id


-- | Makes a path based on some mathematical function
mathPath :: (Ord b, Num n) => ((n,n) -> b) -> IndexPath [] (n,n)
mathPath fn (oX,oY) = sortOn (fn . bimap (subtract oX) (subtract oY))


-- | Makes an expanding diamond shape from origin
diamondPath :: (Num n, Ord n) => IndexPath [] (n,n)
diamondPath
    = mathPath (\(x,y) -> abs x + abs y)
    
    
-- | Makes an expanding diamond shape from origin
diamondPath2 :: (Num n, Ord n) => IndexPath [] (n,n)
diamondPath2
    = mathPath (\(x,y) -> abs x + 2 * abs y)


-- | Divides the plane into two halves around origin
halfPlane :: (Num n, Ord n) => IndexPath [] (n,n)
halfPlane
    = mathPath (\(x,y) -> abs x - abs y)
    
    
-- | Makes an expanding diamond shape from origin
invertedDiamondPath :: (Num n, Ord n) => IndexPath [] (n,n)
invertedDiamondPath
    = mathPath (\(x,y) -> 5000 - abs x - abs y)


-- | Makes an expanding circles around the origin
circularPath :: (Num n, Ord n) => IndexPath [] (n,n)
circularPath
    = mathPath (\(x,y) -> x^2 + y^2)


-- | Makes an expanding square with rounded corners
roundedSquarePath :: (Num n, Ord n) => IndexPath [] (n,n)
roundedSquarePath
    = mathPath (\(x,y) -> x^4 + y^4)


-- | Makes an axis and then expands from it.
-- http://www.wolframalpha.com/input/?i=plot3d+ln(x)+%2B+ln(y)
axisPath :: (Num n, Ord n, Integral n) => IndexPath [] (n,n)
axisPath
    = mathPath (\(x,y) -> log' x + log' y)
    where
        log' = log . fromIntegral . abs


-- | Makes a sort of wavy pattern. Probably best to see it on your own.
-- http://www.wolframalpha.com/input/?i=plot3d+x%5E3+-+9x+%2B+y%5E3+-+9y
wavesPath :: (Num n, Ord n) => IndexPath [] (n,n)
wavesPath
    = mathPath (\(x,y) -> x^3 - 9*x + y^3 - 9*y)


-- | Makes diagonal lines                           
linePath :: (Num n, Ord n) => IndexPath [] (n,n)
linePath
    = mathPath (\(x,y) -> x + y)

    
-- | Makes a saddle around origin
saddleWavePath :: (Num n, Ord n) => IndexPath [] (n,n)
saddleWavePath
    = mathPath (\(x,y) -> x * y)


-- | Makes an interesting pattern
sinCosPath :: (Num n, Ord n, Integral n) => IndexPath [] (n,n)
sinCosPath
    = mathPath ((\(x,y) -> (sin y) / y + (cos x) / x) . bimap fromIntegral fromIntegral)
    

-- | Makes an expanding square spiral from origin
squareSpiralPath :: (Num n, Enum n, Ord n, Integral n) => IndexPath [] (n,n)
squareSpiralPath origin indices
    = take numOfIndices . (origin :) . filter isInside . squareSpiral (Left 1) $ origin
    where (maxX,maxY) = maximum indices
          (minX,minY) = minimum indices
          isInside (x,y) = and [x >= minX, y >= minY, x <= maxX, y <= maxY]
          numOfIndices = fromIntegral $ (maxX - minX + 1) * (maxY - minY + 1)


squareSpiral :: (Num n, Enum n) => Direction n -> (n,n) -> [(n,n)]
squareSpiral (Left n) (x,y)
    = [ (x - i, y) | i <- [1..n] ] ++ squareSpiral (Up n) (x-n,y)
squareSpiral (Up n) (x,y)
    = [ (x, y + i) | i <- [1..n] ] ++ squareSpiral (Right (n+1)) (x,y+n)
squareSpiral (Right n) (x,y)
    = [ (x + i, y) | i <- [1..n] ] ++ squareSpiral (Bottom n) (x+n,y)
squareSpiral (Bottom n) (x,y)
    = [ (x, y - i) | i <- [1..n] ] ++ squareSpiral (Left (n+1)) (x,y-n)


data Direction n
    = Left n
    | Up n
    | Right n
    | Bottom n