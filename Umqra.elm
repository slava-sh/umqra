import Signal (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Text
import Color
import Window
import Keyboard
import Mouse
import Time

type alias Game =
  { player : Player
  }

type alias Object a =
  { a
  | x : Float
  , y : Float
  }

type alias Player = Object
  {
  }

game : Signal Game
game = constant
  { player = { x = 0, y = 0 }
  }

display : (Int, Int) -> Game -> Element
display (w, h) {player} =
  let halfW  = toFloat w / 2
      halfH  = toFloat h / 2
      text s = Text.fromString s
               |> Text.typeface ["Optima", "Helvetica Neue"]
               |> Text.bold
               |> Text.height 22
               |> Text.color Color.white
               |> Text.leftAligned
  in
    layers
      [ collage w h
          [ rect (toFloat w) (toFloat h) |> filled Color.black
          , circle 10 |> filled Color.white |> move (player.x, player.y)
          , circle 3 |> filled Color.red |> move (0, 0)
          ]
      , container w h (topLeftAt (absolute 10) (absolute 10)) (text "Zzz")
      , container w h (midTopAt (relative 0.5) (absolute 10)) (text "Hmm")
      , container w h (topRightAt (absolute 10) (absolute 10)) (text "Arr")
      ]

main : Signal Element
main = display <~ Window.dimensions ~ game
