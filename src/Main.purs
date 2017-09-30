module Main where

import Control.Monad.Eff.Console
import Math (pi)
import Prelude

circleArea n = n * n * pi

main = logShow (circleArea 6.0)
