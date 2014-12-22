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
import Touch
import Touch (Touch)

import Utils (..)

gameFPS : Int
gameFPS = 35

agePS : Float
agePS = 1 / 5

trailsPS : Float
trailsPS = 10

trailLength : Int
trailLength = 15

newDotsExpectedPS : Float
newDotsExpectedPS = 2

cameraFrameSize : Float
cameraFrameSize = 100

playerMinVelocity : Float
playerMinVelocity = 25 / second

playerMaxVelocity : Float
playerMaxVelocity = 75 / second

playerdVelocity : Float
playerdVelocity = 25 / second ^ 2

playerdAngle : Float
playerdAngle = pi / second

playerMinRadius : Float
playerMinRadius = 4

playerMaxRadius : Float
playerMaxRadius = 5

playerBouncePeriod : Time
playerBouncePeriod = 1 * second

dotMinScore : Score
dotMinScore = 1

dotMaxScore : Score
dotMaxScore = 10

dotYoungRadius : Float
dotYoungRadius = 5

dotMatureRadius : Float
dotMatureRadius = 3

coverRadiusToEat : Float
coverRadiusToEat = 2

dotMinLifetime : Time
dotMinLifetime = 5 * second

dotMaxLifetime : Time
dotMaxLifetime = 15 * second

dotEmergenceTime : Float
dotEmergenceTime = 1 * second

dotDyingTime : Float
dotDyingTime = 1 * second

dotMaxVelocity : Float
dotMaxVelocity = 15 / second

dotMaxdAngle : Float
dotMaxdAngle = 2 * pi / second

dotColors : Array Color
dotColors = Array.fromList
  [ Color.red
  , Color.orange
  , Color.yellow
  , Color.green
  , Color.lightRed
  , Color.lightOrange
  , Color.lightYellow
  , Color.lightGreen
  , Color.lightBlue
  ]

type alias Object a = { a | x : Float, y : Float }

type alias Timed a = { a | time : Time }

type alias Game = Timed
  { player  : Player
  , target  : Maybe Target
  , dots    : List Dot
  , seed    : Random.Seed
  }

type alias Point = Object {}

type alias Target = Timed (Object { isActive : Bool })

type alias Player = Object
  { velocity  : Float
  , dVelocity : Float
  , angle     : Float
  , dAngle    : Float
  , trail     : List Point
  , age       : Int
  , score     : Score
  , radius    : Float
  }

type alias Score = Int

type alias Dot = Timed (Object
  { color    : Color
  , state    : DotState
  , lifetime : Time
  , velocity : Float
  , angle    : Float
  , dAngle   : Float
  , radius   : Float
  })

type DotState = Emerging | Ageing | Dying

type alias Camera = Object {}

type alias Scene =
  { game   : Game
  , camera : Camera
  , w      : Float
  , h      : Float
  }

type Update
  = Input Steering
  | Window (Int, Int)
  | NewDot
  | Age
  | Delay Time

type Steering
  = Manual { velocity : Int, angle : Int }
  | Automatic (Maybe Point)

updates : Signal Update
updates = mergeMany
  [ Input         <~ merge (handleTouches <~ Touch.touches)
                           (dropRepeats <| handleArrows <~ Keyboard.arrows)
  , Window        <~ Window.dimensions
  -- A new dot appears with the probability 1/2
  , always NewDot <~ every (second / newDotsExpectedPS / 2)
  , always Age    <~ every (second / agePS)
  , Delay         <~ fps gameFPS
  ]

handleTouches : List Touch -> Steering
handleTouches touches = Automatic <|
  case touches of
    ({ x, y } :: _) -> Just { x = toFloat x, y = toFloat y }
    []              -> Nothing

handleArrows : { x : Int, y : Int } -> Steering
handleArrows { x, y } = Manual
  { velocity = if
      | y == -1   -> -1
      | otherwise ->  1
  , angle = -x
  }

scene : Signal Scene
scene = foldp updateScene defaultScene updates

defaultScene : Scene
defaultScene = readSignal Window.dimensions |> \(w, h) ->
  { game   = defaultGame
  , camera = { x = 0, y = 0 }
  , w      = toFloat w
  , h      = toFloat h
  }

defaultGame : Game
defaultGame = readSignal currentTime |> \time ->
  { seed    = Random.initialSeed <| floor time
  , player  = defaultPlayer
  , target  = Nothing
  , time    = 0
  , dots    =
      [ { x        = 100
        , y        = 0
        , color    = Color.green
        , time     = 0
        , lifetime = 50 * second
        , state    = Emerging
        , velocity = dotMaxVelocity
        , angle    = 0
        , dAngle   = 0
        , radius   = 0
        }
      ]
  }

defaultPlayer : Player
defaultPlayer =
  { x         = 0
  , y         = 0
  , velocity  = playerMinVelocity
  , dVelocity = playerdVelocity
  , angle     = 0
  , dAngle    = 0
  , trail     = []
  , age       = 0
  , score     = 0
  , radius    = 0
  }

randomDot : Float -> Float -> Float -> Float -> Random.Generator Dot
randomDot minX maxX minY maxY = Random.customGenerator <|
  Random.generate (Random.float minX maxX)        `randomThen` \x ->
  Random.generate (Random.float minY maxY)        `randomThen` \y ->
  Random.generate (randomElement dotColors)       `randomThen` \color ->
  Random.generate (Random.float 0 dotMaxVelocity) `randomThen` \velocity ->
  Random.generate (Random.float (-pi) (pi))       `randomThen` \angle ->
  Random.generate (Random.float dotMinLifetime dotMaxLifetime)
    `randomThen` \lifetime ->
  Random.generate (Random.float -dotMaxdAngle dotMaxdAngle)
    `randomThen` \dAngle ->
  randomReturn
    { x        = x
    , y        = y
    , color    = color
    , state    = Emerging
    , time     = 0
    , lifetime = lifetime
    , velocity = velocity
    , angle    = angle
    , dAngle   = dAngle
    , radius   = 0
    }

updateScene : Update -> Scene -> Scene
updateScene update scene =
  let scene' = case update of
        Window (w, h) -> { scene | w <- toFloat w, h <- toFloat h }
        _             -> scene
  in
    scene'
    |> updateGame update
    |> updateCamera

updateCamera : Scene -> Scene
updateCamera ({ game, camera } as scene) =
  let { player, target } = game
      halfFrame = cameraFrameSize / 2
      camera' =
        { camera
        | x <- clamp (player.x - halfFrame) (player.x + halfFrame) camera.x
        , y <- clamp (player.y - halfFrame) (player.y + halfFrame) camera.y
        }
      target' = target |> Maybe.map (\target -> if
                  | target.isActive ->
                      { target
                      | x <- target.x - camera.x + camera'.x
                      , y <- target.y - camera.y + camera'.y
                      }
                  | otherwise -> target)
      game' = { game | target <- target' }
  in { scene | camera <- camera', game <- game' }

updateGame : Update -> Scene -> Scene
updateGame update ({ game, camera } as scene) =
  let { player, dots, target, seed } = game
  in case update of
    Input (Manual { velocity, angle }) ->
      let target' = Nothing
          player' = { player
                    | dVelocity <- toFloat velocity * playerdVelocity
                    , dAngle    <- toFloat angle    * playerdAngle
                    }
          game' = { game | player <- player', target <- target' }
      in { scene | game <- game' }
    Input (Automatic Nothing) ->
      let target' = target
                    |> Maybe.map (\target -> { target | isActive <- False })
          game' = { game | target <- target' }
      in { scene | game <- game' }
    Input (Automatic (Just { x, y })) ->
      let halfW = scene.w / 2
          halfH = scene.h / 2
          target' = Just { x        = x - halfW + camera.x
                         , y        = halfH - y + camera.y
                         , isActive = True
                         , time     = 0
                         }
          player'  = { player | dVelocity <- playerdVelocity }
          game' = { game | target <- target', player <- player' }
      in { scene | game <- game' }
    NewDot ->
      let (newDot, seed') = seed |> Random.generate
            (withProbability 0.5
              (randomDot (player.x - scene.w) (player.x + scene.w)
                         (player.y - scene.h) (player.y + scene.h)))
          dots' = maybeToList newDot ++ dots
          game' = { game | dots <- dots', seed <- seed' }
      in { scene | game <- game' }
    Age ->
      let player' = { player | age <- player.age + 1 }
          game' = { game | player <- player' }
      in { scene | game <- game' }
    Delay dt ->
      let game' = game
                  |> updateTime dt
                  |> updateDots dt
                  |> updatePlayer dt
                  |> updateTarget dt
                  |> eatDots
      in { scene | game <- game' }
    _ -> scene

updateTime : Time -> Timed a -> Timed a
updateTime dt obj = { obj | time <- obj.time + dt }

updateTarget : Time -> Game -> Game
updateTarget dt ({ player } as game) = case game.target of
  Nothing     -> game
  Just target ->
    let targetDistance = distance player target
        target'        = if
          | targetDistance < player.radius -> Nothing
          | otherwise                      -> Just <| updateTime dt target
    in { game | target <- target' }

setState : a -> Timed { obj | state : a } -> Timed { obj | state : a }
setState newState obj = { obj | state <- newState, time <- 0 }

updateDot : Time -> Dot -> Dot
updateDot dt dot =
  let (dx, dy) = fromPolar (dot.velocity, dot.angle)
      x'       = dot.x + dx * dt
      y'       = dot.y + dy * dt
      angle'   = dot.angle + dot.dAngle * dt
      radius'  = case dot.state of
        Emerging -> dotYoungRadius
        Ageing   -> linearScale 0 dot.lifetime
                                dotYoungRadius dotMatureRadius dot.time
        Dying    -> dotMatureRadius
  in
    { dot
    | x      <- x'
    , y      <- y'
    , angle  <- angle'
    , radius <- radius'
    }
    |> updateDotState

updateDotState : Dot -> Dot
updateDotState dot = if
  | dot.state == Emerging && dot.time > dotEmergenceTime -> setState Ageing dot
  | dot.state == Ageing && dot.time > dot.lifetime -> setState Dying dot
  | otherwise -> dot

updateDots : Time -> Game -> Game
updateDots dt ({ dots } as game) =
  let dots' = dots
              |> List.map (updateDot dt << updateTime dt)
              |> List.filter (\dot -> not (dot.state == Dying
                                        && dot.time > dotDyingTime))
  in { game | dots <- dots' }

updatePlayer : Time -> Game -> Game
updatePlayer dt ({ player } as game) =
  let velocity' = player.velocity + player.dVelocity * dt
                  |> clamp playerMinVelocity playerMaxVelocity
      angle'    = case game.target of
        Nothing     -> player.angle + player.dAngle * dt
        Just target ->
          let targetAngle = atan2 (target.y - player.y) (target.x - player.x)
              deltaAngle  = targetAngle - player.angle
                            |> normalizeAngle
                            |> clamp (-playerdAngle * dt) (playerdAngle * dt)
          in player.angle + deltaAngle
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
                  |> updateTrail
  in { game | player <- player' }

distance : Object a -> Object b -> Float
distance a b = sqrt <| (a.x - b.x) ^ 2 + (a.y - b.y) ^ 2

eatDots : Game -> Game
eatDots ({ player, dots } as game) =
  let playerEats dot =
        distance player dot < player.radius + dot.radius - coverRadiusToEat
      (eatenDots, dots') = List.partition playerEats dots
      score' = player.score + List.sum (List.map scoreDot eatenDots)
      player' = { player | score <- score' }
  in { game | player <- player', dots <- dots' }

scoreDot : Dot -> Score
scoreDot dot = case dot.state of
  Emerging -> dotMinScore
  Ageing   -> round <| linearScale 0 dot.lifetime
                         (toFloat dotMinScore) (toFloat dotMaxScore) dot.time
  Dying    -> dotMaxScore

updateTrail : Player -> Player
updateTrail ({ trail } as player) =
  let trail' = if
        | List.isEmpty trail -> [{ x = player.x, y = player.y }]
        | distance player (List.head trail) >
            player.velocity * second / trailsPS ->
              List.take trailLength <| { x = player.x, y = player.y } :: trail
        | otherwise -> trail
  in { player | trail <- trail' }

display : Scene -> Element
display ({ game, camera } as scene) =
  let { player, dots } = game
      w = floor scene.w
      h = floor scene.h
      halfW = scene.w / 2
      halfH = scene.h / 2
      isVisible { x, y, radius } = abs (x - camera.x) - radius < halfW
                                && abs (y - camera.y) - radius < halfH
      text s = Text.fromString s
               |> Text.typeface ["Optima", "Helvetica Neue"]
               |> Text.bold
               |> Text.height 22
               |> Text.color Color.white
               |> Text.leftAligned
      dotForm dot = move (dot.x, dot.y) <| case dot.state of
        Emerging -> circle dot.radius
                    |> filled dot.color
                    |> alpha (linearScale 0 dotEmergenceTime 0 1 dot.time)
        Ageing   -> circle dot.radius
                    |> filled dot.color
        Dying    -> circle dot.radius
                    |> filled dot.color
                    |> alpha (linearScale 0 dotDyingTime 1 0 dot.time)
      scaleTrail = linearScale 0 <| toFloat <| List.length player.trail
      trailForm i dot =
        circle (scaleTrail 2.5 1 <| toFloat i)
        |> filled Color.white
        |> alpha (scaleTrail 1 0 <| toFloat i)
        |> move (dot.x, dot.y)
      targetForm target =
        circle playerMinRadius
        |> filled Color.white
        |> move (target.x, target.y)
      playerForm =
        circle player.radius
        |> filled Color.white
        |> move (player.x, player.y)
  in
    layers
      [ spacer w h |> color Color.black
      , collage w h <| List.map (move (-camera.x, -camera.y)) <|
          List.indexedMap trailForm player.trail ++
          List.map dotForm (List.filter isVisible dots) ++
          [playerForm] ++
          List.map targetForm (maybeToList game.target)
      , container w h (topLeftAt (absolute 10) (absolute 10))  <| text <|
          "Age: " ++ toString player.age
      , container w h (midTopAt (relative 0.5) (absolute 10))  <|
          let fadeTime = 1 / 4 * second
              timedText (start, end, str) = if
                | start < game.time && game.time < end ->
                    text str
                    |> opacity (if
                         | game.time > end - fadeTime ->
                             linearScale fadeTime 0 1 0 <| end - game.time
                         | game.time < start + fadeTime ->
                             linearScale 0 fadeTime 0 1 <| game.time - start
                         | otherwise -> 1)
                    |> Just
                | otherwise -> Nothing
          in
            [ ( 0 * second,  5 * second, "Eat dots to score points")
            , ( 5 * second, 10 * second, "Smaller dots give you more points")
            , (10 * second, 15 * second, "Hold &darr; to maneuver")
            , (15 * second, 18 * second, "Enjoy!")
            ]
            |> List.map timedText
            |> Maybe.oneOf
            |> Maybe.withDefault empty
      , container w h (topRightAt (absolute 10) (absolute 10)) <| text <|
          "Score: " ++ toString player.score
      ]

main : Signal Element
main = display <~ scene

port title : String
port title = "Eat 'Em All"
