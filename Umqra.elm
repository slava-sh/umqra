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
trailsPS = 5

newDotsExpectedPS : Float
newDotsExpectedPS = 2

cameraFrameSize : Float
cameraFrameSize = 300

trailLength : Int
trailLength = 20

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

type alias Game =
  { player  : Player
  , targets : List Point
  , dots    : List Dot
  , seed    : Random.Seed
  , time    : Time
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
  , score     : Score
  , radius    : Float
  }

type alias Score = Int

type alias Dot = Object
  { color    : Color
  , state    : DotState
  , time     : Time
  , lifetime : Time
  , velocity : Float
  , angle    : Float
  , dAngle   : Float
  , radius   : Float
  }

type DotState = Emerging | Ageing | Dying

type Update
  = Input { x : Int, y : Int }
  | Targets (List Touch)
  | Window (Int, Int)
  | Trail
  | NewDot
  | Age
  | Delay Time

updates : Signal Update
updates = mergeMany
  [ Input         <~ Keyboard.arrows
  , Targets       <~ Touch.touches
  , Window        <~ Window.dimensions
  , always Trail  <~ every (second / trailsPS)
  -- A new dot appears with the probability 1/2
  , always NewDot <~ every (second / newDotsExpectedPS / 2)
  , always Age    <~ every (second / agePS)
  , Delay         <~ fps gameFPS
  ]

type alias Camera = Object {}

type alias Scene =
  { game   : Game
  , camera : Camera
  , w      : Float
  , h      : Float
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
  , dots    =
      [ { x        = 100
        , y        = 0
        , color    = Color.green
        , time     = 0
        , lifetime = 10 * second
        , state    = Emerging
        , velocity = dotMaxVelocity
        , angle    = 0
        , dAngle   = 0
        , radius   = 0
        }
      ]
  , targets = []
  , time    = 0
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
  let player = game.player
      halfFrame = cameraFrameSize / 2
      camera' =
        { camera
        | x <- clamp (player.x - halfFrame) (player.x + halfFrame) camera.x
        , y <- clamp (player.y - halfFrame) (player.y + halfFrame) camera.y
        }
  in { scene | camera <- camera' }

updateGame : Update -> Scene -> Scene
updateGame update ({ game, camera } as scene) =
  let { player, dots, seed } = game
  in case update of
    Input direction ->
      let player'  = { player
                     | dVelocity <- if
                         | direction.y == -1 -> -playerdVelocity
                         | otherwise         ->  playerdVelocity
                     , dAngle    <- toFloat direction.x * -playerdAngle
                     }
          game' = { game | player <- player' }
      in { scene | game <- game' }
    Targets targets ->
      let halfW = scene.w / 2
          halfH = scene.h / 2
          targets' = targets
                     |> List.map (\{ x, y } ->
                          { x = toFloat x - halfW + camera.x
                          , y = halfH - toFloat y + camera.y
                          })
          player'  = { player | dVelocity <- playerdVelocity }
          game' = { game | targets <- targets', player <- player' }
      in { scene | game <- game' }
    Trail ->
      let player' = updateTrail player.x player.y player
          game' = { game | player <- player' }
      in { scene | game <- game' }
    NewDot ->
      let (newDot, seed') = Random.generate
            (withProbability 0.5
              (randomDot (player.x - scene.w) (player.x + scene.w)
                         (player.y - scene.h) (player.y + scene.h))) seed
          dots' = maybeToList newDot ++ dots
          game' = { game | dots <- dots', seed <- seed' }
      in { scene | game <- game' }
    Age ->
      let player' = { player | age <- player.age + 1 }
          game' = { game | player <- player' }
      in { scene | game <- game' }
    Delay dt ->
      let game' = game
                  |> \game -> { game | time <- game.time + dt }
                  |> updateDots dt
                  |> updatePlayer dt
                  |> eatDots
      in { scene | game <- game' }
    _ -> scene

setState : a
        -> { obj | state : a, time : Time }
        -> { obj | state : a, time : Time }
setState newState obj = { obj | state <- newState, time <- 0 }

updateDot : Time -> Dot -> Dot
updateDot dt dot =
  let (dx, dy) = fromPolar (dot.velocity, dot.angle)
      time'    = dot.time + dt
      x'       = dot.x + dx * dt
      y'       = dot.y + dy * dt
      angle'   = dot.angle + dot.dAngle * dt
      radius'  = case dot.state of
        Emerging -> dotYoungRadius
        Ageing   -> linearScale 0 dot.lifetime
                                dotYoungRadius dotMatureRadius time'
        Dying    -> dotMatureRadius
  in
    { dot
    | time   <- time'
    , x      <- x'
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
              |> List.map (updateDot dt)
              |> List.filter (\dot -> not (dot.state == Dying
                                        && dot.time > dotDyingTime))
  in { game | dots <- dots' }

updatePlayer : Time -> Game -> Game
updatePlayer dt ({ player, targets } as game) =
  let velocity' = player.velocity + player.dVelocity * dt
                  |> clamp playerMinVelocity playerMaxVelocity
      angle'    = if
        | List.isEmpty targets -> player.angle + player.dAngle * dt
        | otherwise            ->
            let target      = List.head targets
                targetAngle = atan2 (target.y - player.y) (target.x - player.x)
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
  in { game | player <- player' }

distanceSquare : Object a -> Object b -> Float
distanceSquare a b = (a.x - b.x) ^ 2 + (a.y - b.y) ^ 2

eatDots : Game -> Game
eatDots ({ player, dots } as game) =
  let playerEats dot = dot.state /= Dying &&
        (let distanceToEat = player.radius + dot.radius - coverRadiusToEat
         in distanceSquare player dot < distanceToEat ^ 2)
      (eatenDots, dots') = List.partition playerEats dots
      score' = player.score + List.sum (List.map scoreDot eatenDots)
      player' = { player | score <- score' }
  in { game | player <- player', dots <- dots' }

scoreDot : Dot -> Score
scoreDot dot = round <| linearScale 0 dot.lifetime
                                    0 (toFloat dotMaxScore) dot.time

updateTrail : Float -> Float -> Player -> Player
updateTrail x y ({ trail } as player) =
  { player | trail <- List.take trailLength <| { x = x, y = y } :: trail }

display : Scene -> Element
display ({ game, camera } as scene) =
  let { player, dots, targets } = game
      w = floor scene.w
      h = floor scene.h
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
      , container w h (midTopAt (relative 0.5) (absolute 10))  <| text <| if
          | game.time <  5 * second -> "Eat dots to score points"
          | game.time < 10 * second -> "Smaller dots give you more points"
          | game.time < 15 * second -> "Hold &darr; to maneuver"
          | game.time < 18 * second -> "Enjoy!"
          | otherwise               -> ""
      , container w h (topRightAt (absolute 10) (absolute 10)) <| text <|
          "Score: " ++ toString player.score
      ]

main : Signal Element
main = display <~ scene

port title : String
port title = "Eat 'Em All"
