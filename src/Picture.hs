module Picture where


import Color

import Prelude hiding (lookup)

import Data.Map.Strict


-- | Represents the picture that is being drawn.
type Picture = Map (Int,Int) Color


-- | Creates an empty picture. (no color is initialized)
emptyPicture :: Picture
emptyPicture = empty


-- | Colors a pixel in the picture.
colorAPixel :: (Int,Int) -> Color -> Picture -> Picture
colorAPixel = insert


-- | Colors pixels in the picture.
colorPixels :: [((Int,Int), Color)] -> Picture -> Picture
colorPixels [] picture
    = picture
colorPixels ((pixel, color) : list) picture
    = colorPixels list $ colorAPixel pixel color picture


-- | Returns the color at the target pixel if it's colored yet. Otherwise returns `Nothing`.
(!) :: Picture -> (Int,Int) -> Maybe Color
picture ! pixel
    = lookup pixel picture


-- | Returns the color at the target pixel if it's colored yet. Otherwise returns the black color.
(!?) :: Picture -> (Int,Int) -> Color
picture !? pixel
    = maybe 0 id (picture Picture.! pixel)