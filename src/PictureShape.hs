module PictureShape where

import Prelude hiding (Left, Right)

import Data.Bifunctor
import Data.List


-- | `PictureShape` is a function that determines the final shape of the picture.
-- 
-- Underhood, it achieves this by ordering the list of indices, so that some get
-- colored before other ones.
type PictureShape t a
    = a -> t a -> t a


-- | `ShapeFn` is a function that converts an index into some other type using which
-- the indices get ordered.
type ShapeFn index ord
    = (index -> ord)


-- #########################
-- ##                     ##
-- ##  Shape combinators  ##
-- ##                     ##
-- #########################

-- | Makes a picture generating shape based on a mathematical function describing it
useShape :: (Ord ord, Num n) => ShapeFn (n,n) ord -> PictureShape [] (n,n)
useShape fn (oY,oX) = sortOn . moveShape oY oX $ fn


-- | Moves the shape so that it's relative origin is at (moveX, moveY)
moveShape :: (Ord ord, Num n) => n -> n -> ShapeFn (n,n) ord -> ShapeFn (n,n) ord
moveShape moveY moveX fn
    = fn . bimap (subtract moveY) (subtract moveX)


-- | Combines two shapes, so that it always places the point on the closest shape
pickClosestShape :: (Ord ord) => ShapeFn a ord -> ShapeFn a ord -> ShapeFn a ord
pickClosestShape f g index
    = min (f index) (g index)


-- | Uses the first function if x is above xThreshold. Otherwise uses the second function.
pickShapeIfXAbove :: (Ord n) => n -> ShapeFn (n,n) ord -> ShapeFn (n,n) ord -> ShapeFn (n,n) ord
pickShapeIfXAbove xThreshold f g
    = \(y,x) -> if x > xThreshold then f (y,x) else g (y,x)


-- | Uses the first function if y is above yThreshold. Otherwise uses the second function.
pickShapeIfYAbove :: (Ord n) => n -> ShapeFn (n,n) ord -> ShapeFn (n,n) ord -> ShapeFn (n,n) ord
pickShapeIfYAbove yThreshold f g
    = \(y,x) -> if y > yThreshold then f (y,x) else g (y,x)


-- | Uses the first function if x/y is positive/negative. Otherwise uses the second function.
pickShapeIfXIsPositive, pickShapeIfXIsNegative, pickShapeIfYIsPositive, pickShapeIfYIsNegative
    :: (Ord n, Num n) => ShapeFn (n,n) ord -> ShapeFn (n,n) ord -> ShapeFn (n,n) ord
pickShapeIfXIsPositive
    = pickShapeIfXAbove 0
pickShapeIfXIsNegative f g
    = pickShapeIfXAbove (-1) g f
pickShapeIfYIsPositive
    = pickShapeIfYAbove 0
pickShapeIfYIsNegative f g
    = pickShapeIfYAbove (-1) g f


-- | Uses the first function abs x + abs y <= n After that uses the second function.
afterN :: (Ord n, Num n) => n -> ShapeFn (n,n) ord -> ShapeFn (n,n) ord -> ShapeFn (n,n) ord
afterN n f g
    = \(y,x) -> if abs x + abs y <= n then f (y,x) else g (y,x)


-- ##############################
-- ##                          ##
-- ##  Actual shape functions  ##
-- ##                          ##
-- ##############################

-- | Makes an expanding diamond shape from origin
diamond :: (Integral n, Ord i, Num i) => n -> n -> ShapeFn (n,n) i
diamond c1 c2
    = fromIntegral . (\(y,x) -> c1 * abs x + c2 * abs y)


-- | Makes an expanding diamond shape from origin
invertedDiamond :: (Integral n, Ord i, Num i) => n -> n -> ShapeFn (n,n) i
invertedDiamond c1 c2
    = fromIntegral . (\(y,x) -> c1 * c2 * 5000 - c1 * abs x - c2 * abs y)


-- | Divides the plane into two halves around origin
halfPlane :: (Integral n, Ord i, Num i) => n -> n -> ShapeFn (n,n) i
halfPlane c1 c2
    = fromIntegral . (\(y,x) -> c1 * abs x - c2 * abs y)


-- | Makes an expanding circles around the origin
circle :: (Integral n, Ord i, Num i) => ShapeFn (n,n) i
circle
    = fromIntegral . (\(y,x) -> x^2 + y^2)


-- | Makes an expanding ellipses around the origin
ellipse :: (Num n, Ord n, Integral n) => Double -> Double -> ShapeFn (n,n) Double
ellipse c1 c2
    = \(y,x) -> (fromIntegral x^2) / c1 + (fromIntegral y^2) / c2


-- | Makes an expanding square with rounded corners
roundedSquare :: (Integral n, Ord i, Num i) => ShapeFn (n,n) i
roundedSquare
    = fromIntegral . (\(y,x) -> x^4 + y^4)


-- | Makes an axis and then expands from it.
-- http://www.wolframalpha.com/input/?i=plot3d+ln(x)+%2B+ln(y)
axis :: (Num n, Ord n, Integral n) => ShapeFn (n,n) Double
axis
    = \(y,x) -> log' (abs x) + log' (abs y)
    where
        log' = log . fromIntegral


-- | Makes a sort of wavy pattern. Probably best to see it on your own.
-- http://www.wolframalpha.com/input/?i=plot3d+x%5E3+-+9x+%2B+y%5E3+-+9y
waves :: (Integral n, Ord i, Num i) => ShapeFn (n,n) i
waves
    = fromIntegral . (\(y,x) -> x^3 - 9*x + y^3 - 9*y)


-- | Makes diagonal lines                           
line :: (Integral n, Ord i, Num i) => n -> n -> ShapeFn (n,n) i
line c1 c2
    = fromIntegral . (\(y,x) -> c1 * x + c2 * y)

    
-- | Makes a saddle around origin
saddleWave :: (Integral n, Ord i, Num i) => ShapeFn (n,n) i
saddleWave
    = fromIntegral . (\(y,x) -> x * y)


-- | Makes an interesting pattern
sinCos :: (Num n, Ord n, Integral n) => ShapeFn (n,n) Double
sinCos
    = (\(y,x) -> (sin y) / y + (cos x) / x) . bimap fromIntegral fromIntegral
    

-- | Makes an expanding square spiral from origin
squareSpiral :: (Num n, Enum n, Ord n, Integral n) => PictureShape [] (n,n)
squareSpiral origin indices
    = take numOfIndices . (origin :) . filter isInside . squareSpiral' (Left 1) $ origin
    where (maxX,maxY) = maximum indices
          (minX,minY) = minimum indices
          isInside (x,y) = and [x >= minX, y >= minY, x <= maxX, y <= maxY]
          numOfIndices = fromIntegral $ (maxX - minX + 1) * (maxY - minY + 1)


squareSpiral' :: (Num n, Enum n) => Direction n -> (n,n) -> [(n,n)]
squareSpiral' (Left n) (x,y)
    = [ (x - i, y) | i <- [1..n] ] ++ squareSpiral' (Up n) (x-n,y)
squareSpiral' (Up n) (x,y)
    = [ (x, y + i) | i <- [1..n] ] ++ squareSpiral' (Right (n+1)) (x,y+n)
squareSpiral' (Right n) (x,y)
    = [ (x + i, y) | i <- [1..n] ] ++ squareSpiral' (Bottom n) (x+n,y)
squareSpiral' (Bottom n) (x,y)
    = [ (x, y - i) | i <- [1..n] ] ++ squareSpiral' (Left (n+1)) (x,y-n)


data Direction n
    = Left n
    | Up n
    | Right n
    | Bottom n