module Example.Rectangle where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Example.Shapes (translate)
import Graphics.Canvas (CANVAS, moveTo, lineTo, closePath, fillPath, getCanvasElementById, getContext2D, rect, arc, setFillStyle)
import Math as Math
import Partial.Unsafe (unsafePartial)

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- setFillStyle "#0000FF" ctx

-- (Easy) The fillPath and strokePath functions can be used to render complex paths with a common style by using a do notation block inside the function argument. Try changing the Rectangle example to render two rectangles side-by-side using the same call to fillPath. Try rendering a sector of a circle by using a combination of a piecewise-linear path and an arc segment.
  fillPath ctx $ do
    let x = { x: 250.0
            , y: 250.0
            , w: 100.0
            , h: 100.0
            }
    let arcY = { x: 80.0
               , y: 80.0
               , r: 40.0
               , start  : Math.pi * 5.0 / 8.0
               , end    : Math.pi * 2.0
               }

    _ <- rect ctx x
    _ <- rect ctx $ translate 200.0 200.0 x
    _ <- moveTo ctx 80.0 80.0
    _ <- lineTo ctx 260.0 340.0
    _ <- lineTo ctx 340.0 340.0
    _ <- closePath ctx
    arc ctx arcY

