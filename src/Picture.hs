module Picture where


import Color

import Prelude hiding (lookup)

import Data.Map.Strict


type Picture = Map (Int,Int) Color


emptyScreen :: Picture
emptyScreen = empty


addColoredPixel :: (Int,Int) -> Color -> Picture -> Picture
addColoredPixel = insert


addColoredPicture :: [((Int,Int), Color)] -> Picture -> Picture
addColoredPicture [] picture
    = picture
addColoredPicture ((location, color) : list) picture
    = addColoredPicture list $ addColoredPixel location color picture
    
    
(!) :: Picture -> (Int,Int) -> Maybe Color
picture ! pixel = lookup pixel picture

