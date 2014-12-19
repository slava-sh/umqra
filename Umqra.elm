import Signal (..)
import Time (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import List
import Text
import Color
import Window
import Keyboard
import Mouse

type alias Game =
  { player : Player
  }

type alias Object a =
  { a
  | x : Float
  , y : Float
  }

type alias Player = Object
  { velocity : Float
  , angle    : Float
  , angleV   : Float
  }

type Update = Input Int | Delay Time

updates : Signal Update
updates = mergeMany
  [ Input <~ map .x Keyboard.arrows
  , Delay <~ fps 35
  ]

step : Update -> Game -> Game
step update ({player} as game) = case update of
  Input direction ->
    let player'  = { player
                   | angleV <- negate <| toFloat direction * pi / second
                   }
    in { game | player <- player' }
  Delay dt ->
    let angle'   = player.angle + player.angleV * dt
        (dx, dy) = fromPolar (player.velocity, angle')
        player'  = { player
                   | x <- player.x + dx * dt
                   , y <- player.y + dy * dt
                   , angle <- angle'
                   }
    in { game | player <- player' }

defaultGame : Game
defaultGame =
  { player = { x = 0, y = 0, velocity = 100 / second, angle = 0, angleV = 0 }
  }

game : Signal Game
game = foldp step defaultGame updates

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
      camera =
        { x = player.x
        , y = player.y
        }
  in
    layers
      [ spacer w h |> color Color.black
      , collage w h <| List.map (move (-camera.x, -camera.y))
          [ circle 10 |> filled Color.white |> move (player.x, player.y)
          , circle 3 |> filled Color.red
            |> move (player.x, player.y)
            |> move (fromPolar (7, player.angle))
          , circle 3 |> filled Color.red |> move (0, 0)
          ]
      , container w h (topLeftAt (absolute 10) (absolute 10)) (text "Zzz")
      , container w h (midTopAt (relative 0.5) (absolute 10)) (text "Hmm")
      , container w h (topRightAt (absolute 10) (absolute 10)) (text "Arr")
      ]

main : Signal Element
main = display <~ Window.dimensions ~ game
