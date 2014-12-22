module Utils where

import Signal (..)
import Time (..)
import Array
import Array (Array)
import Maybe
import Random

import Native.MyUtils

readSignal : Signal a -> a
readSignal = Native.MyUtils.readSignal

fromJust : Maybe a -> a
fromJust maybe = case maybe of
  Just x -> x

maybeToList : Maybe a -> List a
maybeToList maybe = case maybe of
  Just x  -> [x]
  Nothing -> []

linearScale : Float -> Float -> Float -> Float -> Float -> Float
linearScale oldMin oldMax newMin newMax x =
  (newMax - newMin) / (oldMax - oldMin) * (x - oldMin) + newMin

normalizeAngle : Float -> Float
normalizeAngle angle = if
  | angle >  pi -> angle - 2 * pi
  | angle < -pi -> angle + 2 * pi
  | otherwise   -> angle

type alias RandomM a = Random.Seed -> (a, Random.Seed)

randomReturn : a -> RandomM a
randomReturn x = \seed -> (x, seed)

randomThen : RandomM a -> (a -> RandomM b) -> RandomM b
randomThen m f = \seed -> let (x, seed') = m seed in f x seed'

withProbability : Float -> Random.Generator a -> Random.Generator (Maybe a)
withProbability p g = Random.customGenerator <|
  Random.generate (Random.float 0 1) `randomThen` \chance -> if
    | p < chance -> randomReturn Nothing
    | otherwise  -> Random.generate g `randomThen` \x -> randomReturn <| Just x

{- The array should be non-empty -}
randomElement : Array a -> Random.Generator a
randomElement arr = Random.customGenerator <|
  Random.generate (Random.int 0 <| Array.length arr - 1) `randomThen` \index ->
  randomReturn <| fromJust <| Array.get index arr

currentTime : Signal Time
currentTime = fst <~ timestamp (constant ())
