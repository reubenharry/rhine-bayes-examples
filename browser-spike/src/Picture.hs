-- | Gloss-shaped picture ADT for the browser canvas backend (no OpenGL).
module Picture where

data Color = RGBA
  { redChannel :: Float
  , greenChannel :: Float
  , blueChannel :: Float
  , alphaChannel :: Float
  }
  deriving (Eq, Show)

black, white, red, green, blue, yellow, violet :: Color
black = RGBA 0 0 0 1
white = RGBA 1 1 1 1
red = RGBA 1 0 0 1
green = RGBA 0 1 0 1
blue = RGBA 0 0 1 1
yellow = RGBA 1 1 0 1
violet = RGBA 0.5 0 1 1

makeColorI :: Int -> Int -> Int -> Int -> Color
makeColorI r g b a =
  RGBA
    (fromIntegral r / 255)
    (fromIntegral g / 255)
    (fromIntegral b / 255)
    (fromIntegral a / 255)

withAlpha :: Float -> Color -> Color
withAlpha a (RGBA r g b _) = RGBA r g b a

data Picture
  = Blank
  | Circle Float
  | CircleSolid Float
  | Line [(Float, Float)]
  | Polygon [(Float, Float)]
  | Text String
  | RectangleSolid Float Float
  | RectangleWire Float Float
  | Colored Color Picture
  | Translate Float Float Picture
  | Scale Float Float Picture
  | Rotate Float Picture -- degrees, clockwise like Gloss
  | Pictures [Picture]
  deriving (Eq, Show)

instance Semigroup Picture where
  Blank <> p = p
  p <> Blank = p
  Pictures ps <> Pictures qs = Pictures (ps <> qs)
  Pictures ps <> p = Pictures (ps <> [p])
  p <> Pictures ps = Pictures (p : ps)
  p <> q = Pictures [p, q]

instance Monoid Picture where
  mempty = Blank

circle :: Float -> Picture
circle = Circle

circleSolid :: Float -> Picture
circleSolid = CircleSolid

line :: [(Float, Float)] -> Picture
line = Line

polygon :: [(Float, Float)] -> Picture
polygon = Polygon

text :: String -> Picture
text = Text

rectangleSolid :: Float -> Float -> Picture
rectangleSolid = RectangleSolid

rectangleWire :: Float -> Float -> Picture
rectangleWire = RectangleWire

color :: Color -> Picture -> Picture
color = Colored

translate :: Float -> Float -> Picture -> Picture
translate = Translate

scale :: Float -> Float -> Picture -> Picture
scale = Scale

rotate :: Float -> Picture -> Picture
rotate = Rotate

pictures :: [Picture] -> Picture
pictures = Pictures
