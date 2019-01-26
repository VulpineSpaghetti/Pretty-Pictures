module Picture where


import Color

import Prelude hiding (lookup)

import Data.Map.Strict


type Picture = Map (Int,Int) Color


emptyPicture :: Picture
emptyPicture = empty


addAPixel :: (Int,Int) -> Color -> Picture -> Picture
addAPixel = insert


addPixels :: [((Int,Int), Color)] -> Picture -> Picture
addPixels [] picture
    = picture
addPixels ((location, color) : list) picture
    = addPixels list $ addAPixel location color picture
    
    
(!) :: Picture -> (Int,Int) -> Maybe Color
picture ! pixel = lookup pixel picture

