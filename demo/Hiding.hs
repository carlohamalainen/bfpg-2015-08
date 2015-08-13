module Hiding where

import Data.List hiding (map)
import System.Environment (getArgs)


m = map (+1) [1, 2, 3]

h = head [1, 2, 3]
