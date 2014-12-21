module Utils where

import Native.MyUtils

readSignal : Signal a -> a
readSignal = Native.MyUtils.readSignal
