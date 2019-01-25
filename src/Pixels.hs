module Pixels where


import Color

import Prelude hiding (lookup)

import Data.Map.Strict


type Pixels = Map (Int,Int) Color


emptyScreen :: Pixels
emptyScreen = empty


addColoredPixel :: (Int,Int) -> Color -> Pixels -> Pixels
addColoredPixel = insert


addColoredPixels :: [((Int,Int), Color)] -> Pixels -> Pixels
addColoredPixels [] pixels
    = pixels
addColoredPixels ((location, color) : list) pixels
    = addColoredPixels list $ addColoredPixel location color pixels
    
    
(!) :: Pixels -> (Int,Int) -> Maybe Color
pixels ! pixel = lookup pixel pixels

