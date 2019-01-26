module ColorCombinator where

import Color

import Data.List

import Linear.V3


-- | `ColorCombinator` is a function that takes few colors and combines them to create
-- a new color
type ColorCombinator t a
    = t a -> a


-- | Takes a ColorCombinator function and makes it return the maximum number that can be
-- represented by the given type if the list of colors is empty. Otherwise just runs the
-- ColorCombinator function as normal
ifNotNull :: (Foldable t, Num n, Bounded n) => ColorCombinator t n -> ColorCombinator t n
ifNotNull f xs
    | null xs
        = maxBound
    | otherwise
        = f xs


-- | Takes an average from integral types
averageI :: (Integral n, Bounded n) => ColorCombinator [] n
averageI
    = ifNotNull (\colors -> 
        sum colors `div` genericLength colors)


-- | Takes an average from fractional types
averageF :: (Fractional n, Bounded n) => ColorCombinator [] n
averageF 
    = ifNotNull (\colors ->
        sum colors / genericLength colors)


-- | Picks the median color
median :: (Integral n, Bounded n) => ColorCombinator [] (V3 n)
median
    = ifNotNull (\colors ->
        sortOn (colorDist 0) colors !! (length colors `div` 2))


-- | Picks the darkest color (based on distance from black)
darkestColor :: (Integral n, Bounded n) => ColorCombinator [] (V3 n)
darkestColor
    = ifNotNull (\colors -> 
        head $ sortOn (colorDist 0) colors)
        

-- | Picks the brightest color (based on distance from black)
brightestColor :: (Integral n, Bounded n) => ColorCombinator [] (V3 n)
brightestColor
    = ifNotNull (\colors -> 
        last $ sortOn (colorDist 0) colors)


-- | Takes the brightest and darkest color (also based on distance from black) and takes an average from them.
darkBrightMix :: (Integral n, Bounded n) => ColorCombinator [] (V3 n)
darkBrightMix
    = ifNotNull (\colors -> 
        let min = last $ sortOn (colorDist 0) colors
            max = head $ sortOn (colorDist 0) colors
        in
        (min + max) `div` 2)