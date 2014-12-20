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

gameFPS : Int
gameFPS = 35

agePS : Float
agePS = 1 / 5

maxDotAge : Int
maxDotAge = 3

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
  , score     : Int
  , radius    : Float
  }

type alias Dot = Object
  { color    : Color
  , age      : Int
  , maxAge   : Int
  , state    : DotState
  , time     : Time
  , velocity : Float
  , angle    : Float
  , dAngle   : Float
  }

type DotState = Emerging | Ageing | Dying

type Update
  = Input { x : Int, y : Int }
  | Targets (List Touch) (Int, Int)
  | Trail
  | NewDot
  | Age
  | Delay Time

updates : Signal Update
updates = mergeMany
  [ Input         <~ Keyboard.arrows
  , Targets       <~ Touch.touches ~ Window.dimensions
  , always Trail  <~ every (second / trailsPS)
  -- A new dot appears with the probability 1/2
  , always NewDot <~ every (second / newDotsExpectedPS / 2)
  , always Age    <~ every (second / agePS)
  , Delay         <~ fps gameFPS
  ]

linearScale : Float -> Float -> Float -> Float -> Float -> Float
linearScale oldMin oldMax newMin newMax x =
  (newMax - newMin) / (oldMax - oldMin) * (x - oldMin) + newMin

withProbability : Float
               -> Random.Generator a
               -> Random.Generator (Maybe a)
withProbability p g = Random.customGenerator <| \seed ->
  let (chance, seed') = Random.generate (Random.float 0 1) seed
  in if
    | p < chance -> (Nothing, seed')
    | otherwise  -> let (x, seed'') = Random.generate g seed'
                    in (Just x, seed'')

fromJust : Maybe a -> a
fromJust maybe = case maybe of
  Just x -> x

maybeToList : Maybe a -> List a
maybeToList maybe = case maybe of
  Just x  -> [x]
  Nothing -> []

{- The array should be non-empty -}
randomElement : Array a -> Random.Generator a
randomElement arr = Random.customGenerator <| \seed->
    let (index, seed') =
          Random.generate (Random.int 0 <| Array.length arr - 1) seed
    in (fromJust <| Array.get index arr, seed)

type alias RandomM a = Random.Seed -> (a, Random.Seed)

randomDot : Random.Generator Dot
randomDot = Random.customGenerator <| \seed ->
  let andThen : RandomM a -> (a -> RandomM b) -> RandomM b
      andThen m f = \seed -> let (x, seed') = m seed in f x seed'
      return : a -> RandomM a
      return x = \seed -> (x, seed)
  in seed |>
    Random.generate (Random.float -newDotX newDotX) `andThen` \x ->
    Random.generate (Random.float -newDotY newDotY) `andThen` \y ->
    Random.generate (randomElement dotColors) `andThen` \color ->
    Random.generate (Random.int 1 maxDotAge) `andThen` \maxAge ->
    Random.generate (Random.float 0 dotMaxVelocity) `andThen` \velocity ->
    Random.generate (Random.float (-pi) (pi)) `andThen` \angle ->
    Random.generate (Random.float -dotMaxdAngle dotMaxdAngle)
      `andThen` \dAngle ->
    return { x        = x
           , y        = y
           , color    = color
           , age      = 0
           , maxAge   = maxAge
           , state    = Emerging
           , time     = 0
           , velocity = velocity
           , angle    = angle
           , dAngle   = dAngle
           }

type alias Camera = Object {}

type alias Scene =
  { game   : Game
  , camera : Camera
  }

scene : Signal Scene
scene = foldp updateScene defaultScene updates

defaultScene : Scene
defaultScene =
  { game   = defaultGame
  , camera = { x = 0, y = 0 }
  }

updateScene : Update -> Scene -> Scene
updateScene update = updateGame update >> updateCamera

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
                     | dVelocity <- toFloat direction.y * playerdVelocity
                     , dAngle    <- toFloat direction.x * -playerdAngle
                     }
          game' = { game | player <- player' }
      in { scene | game <- game' }
    Targets targets (w, h) ->
      let halfW = toFloat w / 2
          halfH = toFloat h / 2
          targets' = targets
                     |> List.map (\{ x, y } ->
                          { x = toFloat x - halfW + camera.x
                          , y = halfH - toFloat y + camera.y
                          })
          game' = { game | targets <- targets' }
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
      let updateAge obj = { obj | age <- obj.age + 1 }
          player'       = updateAge player
          dots'         = List.map updateAge dots
          game' = { game | player <- player', dots <- dots' }
      in { scene | game <- game' }
    Delay dt ->
      let game' = game
                  |> \game -> { game | time <- game.time + dt }
                  |> updateDots dt
                  |> updatePlayer dt
                  |> eatDots
      in { scene | game <- game' }

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
updatePlayer dt ({ player, targets } as game) =
  let velocity' = player.velocity + player.dVelocity * dt
                  |> clamp playerMinVelocity playerMaxVelocity
      angle'    = if
        | List.isEmpty targets -> player.angle + player.dAngle * dt
        | otherwise            ->
            let target = List.head targets
            in toPolar (target.x - player.x, target.y - player.y) |> snd
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
      player' = { player | score <- player.score + List.length eatenDots }
  in { game | player <- player', dots <- dots' }

updateTrail : Float -> Float -> Player -> Player
updateTrail x y ({ trail } as player) =
  { player | trail <- List.take trailLength <| { x = x, y = y } :: trail }

defaultPlayer : Player
defaultPlayer =
  { x         = 0
  , y         = 0
  , velocity  = playerMinVelocity
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
  { player  = defaultPlayer
  , targets = []
  , dots    = []
  , seed    = Random.initialSeed 499
  , time    = 0
  }

dotColors : Array Color
dotColors = Array.fromList
  [ Color.red
  , Color.lightBlue
  , Color.orange
  , Color.lightGreen
  ]

display : (Int, Int) -> Scene -> Element
display (w, h) { game, camera } =
  let { player, dots, targets } = game
      text s = Text.fromString s
               |> Text.typeface ["Optima", "Helvetica Neue"]
               |> Text.bold
               |> Text.height 22
               |> Text.color Color.white
               |> Text.leftAligned
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
main = display <~ Window.dimensions ~ scene
