import Signal (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Text
import Color
import Window
import Keyboard
import Mouse
import Time

type alias Game = {}

game : Signal Game
game = constant {}

display : (Int, Int) -> Game -> Element
display (w, h) _ = collage w h
  [ rect (toFloat w) (toFloat h) |> filled Color.black
  ]

main : Signal Element
main = display <~ Window.dimensions ~ game
