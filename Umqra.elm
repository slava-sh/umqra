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

gameFPS : Int
gameFPS = 35

agePS : Float
agePS = 1 / 5

maxDotAge : Int
maxDotAge = 3

trailFPS : Float
trailFPS = 5

newDotsExpectedPS : Float
newDotsExpectedPS = 1

newDotX : Float
newDotX = 1000

newDotY : Float
newDotY = 600

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

dotEmergenceTime : Float
dotEmergenceTime = 1 * second

dotDyingTime : Float
dotDyingTime = 1 * second

playerMinRadius : Float
playerMinRadius = 4

playerMaxRadius : Float
playerMaxRadius = 5

playerBouncePeriod : Time
playerBouncePeriod = 1 * second

dotRadius : Float
dotRadius = 3

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
  , score     : Int
  , radius    : Float
  }

type alias Dot = Object
  { color  : Color
  , age    : Int
  , maxAge : Int
  , state  : DotState
  , time   : Time
  }

type DotState = Emerging | Ageing | Dying

type Update = Input { x : Int, y : Int} | Trail | NewDot | Age | Delay Time

updates : Signal Update
updates = mergeMany
  [ Input         <~ Keyboard.arrows
  , always Trail  <~ every (second / trailFPS)
  -- A new dot appears with the probability 1/2
  , always NewDot <~ every (second / newDotsExpectedPS / 2)
  , always Age    <~ every (second / agePS)
  , Delay         <~ fps gameFPS
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
  in if
    | p < chance -> (Nothing, seed')
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
  let (x, seed')         = Random.generate (Random.float -newDotX newDotX) seed
      (y, seed'')        = Random.generate (Random.float -newDotY newDotY) seed'
      (color, seed''')   = randomElement dotColors seed''
      (maxAge, seed'''') = Random.generate (Random.int 1 maxDotAge) seed'''
      dot = { x      = x
            , y      = y
            , color  = color
            , age    = 0
            , maxAge = maxAge
            , state  = Emerging
            , time   = 0
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
  NewDot ->
    let (newDot, seed') = withProbability 0.5 generateDot seed
        dots' = maybeToList (Maybe.map relativeToPlayer newDot) ++ dots
        relativeToPlayer obj =
          { obj
          | x <- player.x + obj.x
          , y <- player.y + obj.y
          }
    in { game | dots <- dots', seed <- seed' }
  Age ->
    let updateAge obj = { obj | age <- obj.age + 1 }
        player' = updateAge player
        dots'   = List.map updateAge dots
    in { game | player <- player', dots <- dots' }
  Delay dt -> game
              |> \game -> { game | time <- game.time + dt }
              |> updateDots dt
              |> eatDots
              |> updatePlayer dt

setState : a
        -> { obj | state : a, time : Time }
        -> { obj | state : a, time : Time }
setState newState obj = { obj | state <- newState, time <- 0 }

updateDot : Time -> Dot -> Dot
updateDot dt dot =
  { dot
  | time <- dot.time + dt
  }
  |> updateDotState

updateDotState : Dot -> Dot
updateDotState dot = if
  | dot.state == Emerging && dot.time > dotEmergenceTime ->
      setState Ageing dot
  | dot.state == Ageing && dot.age > dot.maxAge ->
      setState Dying dot
  | otherwise -> dot

updateDots : Time -> Game -> Game
updateDots dt ({ dots } as game) =
  let dots' = dots
              |> List.map (updateDot dt)
              |> List.filter (\dot -> not (dot.state == Dying
                                        && dot.time > dotDyingTime))
  in { game | dots <- dots' }

updatePlayer : Time -> Game -> Game
updatePlayer dt ({player} as game) =
  let velocity' = player.velocity + player.dVelocity * dt
                  |> clamp minPlayerVelocity maxPlayerVelocity
      angle'    = player.angle + player.dAngle * dt
      (dx, dy)  = fromPolar (velocity', angle')
      player'   = { player
                  | x        <- player.x + dx * dt
                  , y        <- player.y + dy * dt
                  , velocity <- velocity'
                  , angle    <- angle'
                  , radius   <-
                      cos (2 * pi / playerBouncePeriod * game.time)
                      |> linearScale 1 -1 playerMinRadius playerMaxRadius
                  }
  in { game | player <- player' }

distanceSquare : Object a -> Object b -> Float
distanceSquare a b = (a.x - b.x) ^ 2 + (a.y - b.y) ^ 2

eatDots : Game -> Game
eatDots ({ player, dots } as game) =
  let playerTouches dot  =
        distanceSquare player dot < (player.radius + dotRadius) ^ 2
      (eatenDots, dots') = List.partition playerTouches dots
      player' = { player | score <- player.score + List.length eatenDots }
  in { game | player <- player', dots <- dots' }

updateTrail : Float -> Float -> Player -> Player
updateTrail x y ({ trail } as player) =
  { player
  | trail <- List.take trailLength <| { x = x, y = y } :: trail
  }

defaultPlayer : Player
defaultPlayer =
  { x         = 0
  , y         = 0
  , velocity  = minPlayerVelocity
  , dVelocity = 0
  , angle     = 0
  , dAngle    = 0
  , trail     = []
  , age       = 0
  , score     = 0
  , radius    = 0
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
      dotForm dot = move (dot.x, dot.y) <| case dot.state of
        Emerging -> circle dotRadius
                    |> filled dot.color
                    |> alpha (linearScale 0 dotEmergenceTime 0 1 dot.time)
        Ageing   -> circle dotRadius
                    |> filled dot.color
        Dying    -> circle dotRadius
                    |> filled dot.color
                    |> alpha (linearScale 0 dotDyingTime 1 0 dot.time)
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
          [ circle player.radius
            |> filled Color.white
            |> move (player.x, player.y)
          ]
      , container w h (topLeftAt (absolute 10) (absolute 10))  <| text <|
          "Age: " ++ toString player.age
      , container w h (midTopAt (relative 0.5) (absolute 10))  <| text "Arr"
      , container w h (topRightAt (absolute 10) (absolute 10)) <| text <|
          "Score: " ++ toString player.score
      ]

main : Signal Element
main = display <~ Window.dimensions ~ game
