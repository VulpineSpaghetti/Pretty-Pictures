module Picture where


import Color

import Prelude hiding (lookup)

import Data.Map.Strict


-- | Represents the picture that is being drawn.
type Picture = Map (Int,Int) Color


-- | Creates an empty picture. (no color is initialized)
emptyPicture :: Picture
emptyPicture = empty


-- | Colors/Adds a pixel into the picture.
addAPixel :: (Int,Int) -> Color -> Picture -> Picture
addAPixel = insert


-- | Colors/Adds pixels into the picture.
addPixels :: [((Int,Int), Color)] -> Picture -> Picture
addPixels [] picture
    = picture
addPixels ((location, color) : list) picture
    = addPixels list $ addAPixel location color picture


-- | Returns the color at the target pixel if it's colored yet. Otherwise returns `Nothing`.
(!) :: Picture -> (Int,Int) -> Maybe Color
picture ! pixel
    = lookup pixel picture


-- | Returns the color at the target pixel if it's colored yet. Otherwise returns the black color.
(!?) :: Picture -> (Int,Int) -> Color
picture !? pixel
    = maybe 0 id (picture Picture.! pixel)