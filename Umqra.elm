import Signal (..)
import Time (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import String
import Array
import Array (Array)
import List
import List ((::))
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

ageFPS : Float
ageFPS = 1 / 5

maxDotAge : Int
maxDotAge = 3

trailFPS : Float
trailFPS = 5

trailLength : Int
trailLength = 20

minPlayerVelocity : Float
minPlayerVelocity = 25 / second

maxPlayerVelocity : Float
maxPlayerVelocity = 75 / second

dVelocity : Float
dVelocity = 25 / second ^ 2

dAngle : Float
dAngle = pi / second

newDotProbability : Float
newDotProbability = 1 / 3 / toFloat gameFPS

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

type alias Point = Object {}

type alias Player = Object
  { velocity  : Float
  , dVelocity : Float
  , angle     : Float
  , dAngle    : Float
  , trail     : List Point
  , age       : Int
  }

type alias Dot = Object
  { color  : Color
  , age    : Int
  , maxAge : Int
  }

type Update = Input { x : Int, y : Int} | Trail | Age | Delay Time

updates : Signal Update
updates = mergeMany
  [ Input        <~ Keyboard.arrows
  , always Trail <~ every (second / trailFPS)
  , always Age   <~ every (second / ageFPS)
  , Delay        <~ fps gameFPS
  ]

linearScale : Float -> Float -> Float -> Float -> Float -> Float
linearScale oldMin oldMax newMin newMax x =
  (newMax - newMin) / (oldMax - oldMin) * (x - oldMin) + newMin

withProbability : Float
               -> (Random.Seed -> (a, Random.Seed))
               -> Random.Seed
               -> (Maybe a, Random.Seed)
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
    let (index, seed') =
          Random.generate (Random.int 0 <| Array.length arr - 1) seed
    in (fromJust <| Array.get index arr, seed)

generateDot : Random.Seed -> (Dot, Random.Seed)
generateDot seed =
  let (x, seed')         = Random.generate (Random.float -200 200) seed
      (y, seed'')        = Random.generate (Random.float -200 200) seed'
      (color, seed''')   = randomElement dotColors seed''
      (maxAge, seed'''') = Random.generate (Random.int 1 maxDotAge) seed'''
      dot = { x      = x
            , y      = y
            , color  = color
            , age    = 0
            , maxAge = maxAge
            }
  in (dot, seed'''')

step : Update -> Game -> Game
step update ({player, dots, seed} as game) = case update of
  Input direction ->
    let player'  = { player
                   | dVelocity <- toFloat direction.y * dVelocity
                   , dAngle    <- toFloat direction.x * -dAngle
                   }
    in { game | player <- player' }
  Trail ->
    let player' = updateTrail player.x player.y player
    in { game | player <- player' }
  Age ->
    let updateAge obj = { obj | age <- obj.age + 1 }
        player' = updateAge player
        dots'   = dots
                  |> List.map updateAge
                  |> List.filter (\dot -> dot.age <= dot.maxAge)
    in { game | player <- player', dots <- dots' }
  Delay dt ->
    let velocity' = player.velocity + player.dVelocity * dt
                    |> clamp minPlayerVelocity maxPlayerVelocity
        angle'    = player.angle + player.dAngle * dt
        (dx, dy)  = fromPolar (velocity', angle')
        player'   = { player
                    | x        <- player.x + dx * dt
                    , y        <- player.y + dy * dt
                    , velocity <- velocity'
                    , angle    <- angle'
                    }
        (newDot, seed') = withProbability newDotProbability generateDot seed
        dots' = maybeToList (Maybe.map relativeToPlayer newDot) ++ dots
        relativeToPlayer obj =
          { obj
          | x <- player.x + obj.x
          , y <- player.y + obj.y
          }
    in
      { game
      | player <- player'
      , dots   <- dots'
      , seed   <- seed'
      , time   <- game.time + dt
      }

updateTrail : Float -> Float -> Player -> Player
updateTrail x y ({ trail } as player) =
  { player
  | trail <- List.take trailLength <| { x = x, y = y } :: trail
  }

defaultPlayer : Player
defaultPlayer =
  { x         = 0
  , y         = 0
  , velocity  = minPlayerVelocity / second
  , dVelocity = 0
  , angle     = 0
  , dAngle    = 0
  , trail     = []
  , age       = 0
  }

defaultGame : Game
defaultGame =
  { player = defaultPlayer
  , dots = []
  , seed = Random.initialSeed 499
  , time = 0
  }

dotColors : Array Color
dotColors = Array.fromList
  [ Color.red
  , Color.lightBlue
  , Color.orange
  , Color.lightGreen
  ]

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
      dotForm dot =
        circle 3
        |> filled dot.color
        |> move (dot.x, dot.y)
      scaleTrail = linearScale 0 <| toFloat (List.length player.trail) - 1
      trailForm i dot =
        circle (scaleTrail 2.5 1 <| toFloat i)
        |> filled Color.white
        |> alpha (scaleTrail 1 0 <| toFloat i)
        |> move (dot.x, dot.y)
  in
    layers
      [ spacer w h |> color Color.black
      , collage w h <| List.map (move (-camera.x, -camera.y)) <|
          List.indexedMap trailForm player.trail ++
          List.map dotForm dots ++
          [ circle 4 |> filled Color.white |> move (player.x, player.y)
          ]
      , container w h (topLeftAt (absolute 10) (absolute 10))  <| text <|
          "Age: " ++ toString player.age
      , container w h (midTopAt (relative 0.5) (absolute 10))  <| text "Hmm"
      , container w h (topRightAt (absolute 10) (absolute 10)) <| text "Arr"
      ]

main : Signal Element
main = display <~ Window.dimensions ~ game
