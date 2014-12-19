import Signal (..)
import Time (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import List
import Text
import Color
import Color (Color)
import Window
import Keyboard
import Mouse

type alias Game =
  { player : Player
  , dots   : List Dot
  }

type alias Object a =
  { a
  | x : Float
  , y : Float
  }

type alias Player = Object
  { velocity  : Float
  , dVelocity : Float
  , angle     : Float
  , dAngle    : Float
  }

type alias Dot = Object
  { color : Color
  }

type Update = Input { x : Int, y : Int} | Delay Time

updates : Signal Update
updates = mergeMany
  [ Input <~ Keyboard.arrows
  , Delay <~ fps 35
  ]

step : Update -> Game -> Game
step update ({player} as game) = case update of
  Input direction ->
    let player'  = { player
                   | dVelocity <- toFloat direction.y * (0.05) / second
                   , dAngle    <- toFloat direction.x * (-pi) / second
                   }
    in { game | player <- player' }
  Delay dt ->
    let velocity' = player.velocity + player.dVelocity * dt
        angle'    = player.angle + player.dAngle * dt
        (dx, dy)  = fromPolar (velocity', angle')
        player'   = { player
                    | x        <- player.x + dx * dt
                    , y        <- player.y + dy * dt
                    , velocity <- velocity'
                    , angle    <- angle'
                    }
    in { game | player <- player' }

defaultGame : Game
defaultGame =
  { player = { x         = 0
             , y         = 0
             , velocity  = 5 / second
             , dVelocity = 0
             , angle     = 0
             , dAngle    = 0
             }
  , dots = [{ x = -30, y = -40, color = Color.lightBlue }]
  }

game : Signal Game
game = foldp step defaultGame updates

display : (Int, Int) -> Game -> Element
display (w, h) {player, dots} =
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
      dotForm dot = circle 3
                    |> filled dot.color
                    |> move (dot.x, dot.y)
  in
    layers
      [ spacer w h |> color Color.black
      , collage w h <| List.map (move (-camera.x, -camera.y)) <|
          List.map dotForm dots ++
          [ circle 10 |> filled Color.white |> move (player.x, player.y)
          , circle 3 |> filled Color.red
            |> move (player.x, player.y)
            |> move (fromPolar (7, player.angle))
          ]
      , container w h (topLeftAt (absolute 10) (absolute 10))  <| text "Zzz"
      , container w h (midTopAt (relative 0.5) (absolute 10))  <| text "Hmm"
      , container w h (topRightAt (absolute 10) (absolute 10)) <| text "Arr"
      ]

main : Signal Element
main = display <~ Window.dimensions ~ game
