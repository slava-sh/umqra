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
newDotsExpectedPS = 1

cameraFrameSize : Float
cameraFrameSize = 300

newDotX : Float
newDotX = 1000

newDotY : Float
newDotY = 600

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

randomDot : Random.Generator Dot
randomDot = Random.customGenerator <|
  Random.generate (Random.float -newDotX newDotX) `randomThen` \x ->
  Random.generate (Random.float -newDotY newDotY) `randomThen` \y ->
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
    }

type alias Camera = Object {}

type alias Scene =
  { game   : Game
  , camera : Camera
  , w      : Int
  , h      : Int
  }

scene : Signal Scene
scene = foldp updateScene defaultScene updates

defaultScene : Scene
defaultScene = readSignal Window.dimensions |> \(w, h) ->
  { game   = defaultGame
  , camera = { x = 0, y = 0 }
  , w      = w
  , h      = h
  }

updateScene : Update -> Scene -> Scene
updateScene update scene =
  let scene' = case update of
        Window (w, h) -> { scene | w <- w, h <- h }
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
      let halfW = toFloat scene.w / 2
          halfH = toFloat scene.h / 2
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
      let (newDot, seed') = Random.generate (withProbability 0.5 randomDot) seed
          dots' = maybeToList (Maybe.map relativeToPlayer newDot) ++ dots
          relativeToPlayer obj = { obj
                                 | x <- player.x + obj.x
                                 , y <- player.y + obj.y
                                 }
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
  in
    { dot
    | time  <- time'
    , x     <- x'
    , y     <- y'
    , angle <- angle'
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
        -- Eat a dot when the player covers its center
        distanceSquare player dot < player.radius ^ 2
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

defaultGame : Game
defaultGame = readSignal currentTime |> \time ->
  { player  = defaultPlayer
  , targets = []
  , dots    = []
  , seed    = Random.initialSeed <| floor time
  , time    = 0
  }

dotColors : Array Color
dotColors = Array.fromList
  [ Color.red
  , Color.lightBlue
  , Color.orange
  , Color.lightGreen
  ]

display : Scene -> Element
display { game, camera, w, h } =
  let { player, dots, targets } = game
      text s = Text.fromString s
               |> Text.typeface ["Optima", "Helvetica Neue"]
               |> Text.bold
               |> Text.height 22
               |> Text.color Color.white
               |> Text.leftAligned
      dotForm dot = move (dot.x, dot.y) <| case dot.state of
        Emerging -> circle dotYoungRadius
                    |> filled dot.color
                    |> alpha (linearScale 0 dotEmergenceTime 0 1 dot.time)
        Ageing   -> circle (linearScale 0 dot.lifetime
                                        dotYoungRadius dotMatureRadius dot.time)
                    |> filled dot.color
        Dying    -> circle dotMatureRadius
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
      , container w h (midTopAt (relative 0.5) (absolute 10))  <| text <|
          "Smaller dots give more points"
      , container w h (topRightAt (absolute 10) (absolute 10)) <| text <|
          "Score: " ++ toString player.score
      ]

main : Signal Element
main = display <~ scene
