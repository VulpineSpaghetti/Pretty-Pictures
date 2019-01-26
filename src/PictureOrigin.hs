module PictureOrigin where


-- | The origin from which the picture will be created. Namely IndexPath will treat this point as origin.
data PictureOrigin
    = TopLeft
    | TopMiddle
    | TopRight
    | MiddleLeft
    | Center
    | MiddleRight
    | BottomLeft
    | BottomMiddle
    | BottomRight
    | PictureOrigin Double Double
    deriving (Show)


-- | Converts the origin into a pair of doubles in the range [0,1] representing its position
getRelativePos :: PictureOrigin -> (Double, Double)
getRelativePos TopLeft           = (0  , 0  )
getRelativePos TopMiddle         = (0.5, 0  )
getRelativePos TopRight          = (1  , 0  )
getRelativePos MiddleLeft        = (0  , 0.5)
getRelativePos Center            = (0.5, 0.5)
getRelativePos MiddleRight       = (1  , 0.5)
getRelativePos BottomLeft        = (0  , 1  )
getRelativePos BottomMiddle      = (0.5, 1  )
getRelativePos BottomRight       = (1  , 1  )
getRelativePos (PictureOrigin x y) = (x  , y  )


-- | Converts the origin into an exact point in rectangle bounded by (0,0) and (x,y)
getAbsolutePos :: PictureOrigin -> (Int, Int) -> (Int, Int)
getAbsolutePos pictureOrigin (x,y)
    = (round (fromIntegral x * aX), round (fromIntegral y * aY))
    where (aX,aY) = getRelativePos pictureOrigin
