module ColorCombinator where

import Color

import Data.List

import Linear.V3


-- | `ColorCombinator` is a function that takes few colors and combines them to create
-- a new color
type ColorCombinator t a = (t a -> a)


ifNotNull :: (Foldable t, Num n, Bounded n) => ColorCombinator t n -> ColorCombinator t n
ifNotNull f xs
    | null xs
        = maxBound
    | otherwise
        = f xs


averageI :: (Integral n, Bounded n) => ColorCombinator [] n
averageI
    = ifNotNull (\colors -> 
        sum colors `div` genericLength colors)


averageF :: (Fractional n, Bounded n) => ColorCombinator [] n
averageF 
    = ifNotNull (\colors ->
        sum colors / genericLength colors)


median :: (Integral n, Bounded n) => ColorCombinator [] (V3 n)
median
    = ifNotNull (\colors ->
        sortOn (colorDist 0) colors !! (length colors `div` 2))
    
    
minColor :: (Integral n, Bounded n) => ColorCombinator [] (V3 n)
minColor
    = ifNotNull (\colors -> 
        head $ sortOn (colorDist 0) colors)
        

maxColor :: (Integral n, Bounded n) => ColorCombinator [] (V3 n)
maxColor
    = ifNotNull (\colors -> 
        last $ sortOn (colorDist 0) colors)


minMax :: (Integral n, Bounded n) => ColorCombinator [] (V3 n)
minMax
    = ifNotNull (\colors -> 
        let min = last $ sortOn (colorDist 0) colors
            max = head $ sortOn (colorDist 0) colors
        in
        (min + max) `div` 2)