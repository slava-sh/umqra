import Signal (..)
import Time (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Array
import Array (Array)
import List
import Maybe
import Random
import Text
import Color
import Color (Color)
import Window
import Keyboard
import Mouse

gameFPS : Int
gameFPS = 35

newDotProbability : Float
newDotProbability = 1 / toFloat gameFPS

type alias Game =
  { player : Player
  , dots   : List Dot
  , seed   : Random.Seed
  , time   : Time
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
  , Delay <~ fps gameFPS
  ]

withProbability : Float -> (Random.Seed -> (a, Random.Seed)) -> Random.Seed -> (Maybe a, Random.Seed)
withProbability p g seed =
  let (chance, seed') = Random.generate (Random.float 0 1) seed
  in if | p < chance -> (Nothing, seed')
        | otherwise  -> let (x, seed'') = g seed' in (Just x, seed'')

fromJust : Maybe a -> a
fromJust maybe = case maybe of
  Just x -> x

maybeToList : Maybe a -> List a
maybeToList maybe = case maybe of
  Just x  -> [x]
  Nothing -> []

{- The array should be non-empty -}
randomElement : Array a -> Random.Seed -> (a, Random.Seed)
randomElement arr seed =
    let (index, seed') = Random.generate (Random.int 0 <| Array.length arr - 1) seed
    in (fromJust <| Array.get index arr, seed)

generateDot : Random.Seed -> (Dot, Random.Seed)
generateDot seed =
  let (x, seed')       = Random.generate (Random.float -500 500) seed
      (y, seed'')      = Random.generate (Random.float -500 500) seed'
      (color, seed''') = randomElement dotColors seed''
      dot = { x     = x
            , y     = y
            , color = color
            }
  in (dot, seed''')

step : Update -> Game -> Game
step update ({player, dots, seed} as game) = case update of
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
        (newDot, seed') = withProbability newDotProbability generateDot seed
        dots'           = maybeToList newDot ++ dots
    in { game
       | player <- player'
       , dots   <- dots'
       , seed   <- seed'
       , time   <- game.time + dt
       }

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
  , seed = Random.initialSeed 499
  , time = 0
  }

dotColors : Array Color
dotColors = Array.fromList [Color.red, Color.lightBlue, Color.orange, Color.lightGreen]

game : Signal Game
game = foldp step defaultGame updates

display : (Int, Int) -> Game -> Element
display (w, h) ({player, dots} as game) =
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
      , container w h (topLeftAt (absolute 10) (absolute 10))  <| text <|
          toString (floor <| inSeconds game.time) ++ " " ++ toString (List.length dots)
      , container w h (midTopAt (relative 0.5) (absolute 10))  <| text "Hmm"
      , container w h (topRightAt (absolute 10) (absolute 10)) <| text "Arr"
      ]

main : Signal Element
main = display <~ Window.dimensions ~ game
