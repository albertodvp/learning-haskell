module Lib where

import System.Random
import System.Random.Internal


newtype State s a = State { runState :: s -> (a, s) }

