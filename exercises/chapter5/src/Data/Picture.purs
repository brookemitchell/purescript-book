module Data.Picture where

import Prelude

import Data.Foldable (foldl)
import Global as Global
import Math as Math

data Point = Point
  { x :: Number
  , y :: Number
  }

  <>

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

-- showShape :: Shape -> String
-- showShape (Circle c r) =
--   "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
-- showShape (Rectangle c w h) =
--   "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
-- showShape (Line start end) =
--   "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
-- showShape (Text loc text) =
--   "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
-- showShape (Clipped loc picture) =
--   "Clipped [location: " <> showPoint loc <> ", clipped: " <> (foldl (<>) "" $ map showShape picture) <> "]"

showPicture :: Picture -> Array String
showPicture = map show


data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped _ picture) = bounds picture

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

-- (Medium) Extend the vector graphics library with a new operation area which computes the area of a Shape. For the purposes of this exercise, the area of a piece of text is assumed to be zero.
area :: Shape -> Number
area (Circle _ rad) = Math.pow 2.0 $ Math.pi * rad
area (Rectangle _ h w) = h * w
area _ = 0.0

type Picture = Array Shape
-- (Difficult) Extend the Shape type with a new data constructor Clipped, which clips another Picture to a rectangle. Extend the shapeBounds function to compute the bounds of a clipped picture. Note that this makes Shape into a recursive data type.
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Point Picture

instance showShape :: Show Shape where
  show (Circle c r) =
    "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
  show (Rectangle c w h) =
    "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
  show (Line start end) =
    "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
  show (Text loc text) =
    "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
  show (Clipped loc picture) =
    "Clipped [location: " <> showPoint loc <> ", clipped: " <> (foldl (<>) "" $ map show picture) <> "]"
