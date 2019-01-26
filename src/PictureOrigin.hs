module PictureOrigin where


-- | The origin from which the picture will be created. Namely IndexPath will treat this point as origin.
data PictureOrigin
    = TopLeft
    | TopMiddle
    | TopRight
    | MiddleLeft
    | TheMiddle
    | MiddleRight
    | BottomLeft
    | BottomMiddle
    | BottomRight
    | PictureOrigin Double Double
    deriving (Show)


-- | Converts the origin into a pair of doubles in the range [0,1] representing its position
getRelativePos :: PictureOrigin -> (Double, Double)
getRelativePos TopLeft           = (0  , 1  )
getRelativePos TopMiddle         = (0.5, 1  )
getRelativePos TopRight          = (1  , 1  )
getRelativePos MiddleLeft        = (0  , 0.5)
getRelativePos TheMiddle         = (0.5, 0.5)
getRelativePos MiddleRight       = (1  , 0.5)
getRelativePos BottomLeft        = (0  , 0  )
getRelativePos BottomMiddle      = (0  , 0.5)
getRelativePos BottomRight       = (0  , 1  )
getRelativePos (PictureOrigin x y) = (x  , y  )


-- | Converts the origin into an exact point in rectangle bounded by (0,0) and (x,y)
getAbsolutePos :: PictureOrigin -> (Int, Int) -> (Int, Int)
getAbsolutePos pictureOrigin (x,y)
    = (round (fromIntegral x * aX), round (fromIntegral y * aY))
    where (aX,aY) = getRelativePos pictureOrigin
