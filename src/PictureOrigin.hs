module PictureOrigin where


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


getAbsolutePos :: PictureOrigin -> (Int, Int) -> (Int, Int)
getAbsolutePos pictureOrigin (x,y)
    = (round (fromIntegral x * aX), round (fromIntegral y * aY))
    where (aX,aY) = getRelativePos pictureOrigin
