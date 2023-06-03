{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
-- ghc is broken in this analysis in numberOfWheels' function :shrug:
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Expresion where

import           Data.Type.Equality (type (==))

data Car = Car
    { carName           :: String
    , carSpeed          :: Int
    , carNumberOfWheels :: Int
    }
    deriving (Eq, Show)

data Motorcycle = Motorcycle
    { motorcycleName           :: String
    , motorcycleSpeed          :: Int
    , motorcycleNumberOfWheels :: Int
    }
    deriving (Eq, Show)

data Sleight = Sleight
    { sleightName  :: String
    , sleightSpeed :: Int
    }
    deriving (Eq, Show)

-- open world solution

class Vehicle a where
    name :: a -> String
    speed :: a -> Int

instance Vehicle Car where
    name = carName
    speed = carSpeed

instance Vehicle Motorcycle where
    name = motorcycleName
    speed = motorcycleSpeed

class NumberOfWheels a where
    numberOfWheels :: a -> Int

instance NumberOfWheels Car where
    numberOfWheels = carNumberOfWheels

instance NumberOfWheels Motorcycle where
    numberOfWheels = motorcycleNumberOfWheels

-- closed world solution
-- TODO. solve it without GADTs, observe the limitation

data VehicleT a where
    CarT :: VehicleT Car
    MotorcycleT :: VehicleT Motorcycle
    SleightT :: VehicleT Sleight

name' :: VehicleT a -> a -> String
name' CarT c        = carName c
name' MotorcycleT m = motorcycleName m
name' SleightT s    = sleightName s

speed' :: VehicleT a -> a -> Int
speed' CarT c        = carSpeed c
speed' MotorcycleT m = motorcycleSpeed m
speed' SleightT s    = sleightSpeed s

numberOfWheels' :: (a == Sleight) ~ 'False => VehicleT a -> a -> Int
numberOfWheels' CarT c        = carNumberOfWheels c
numberOfWheels' MotorcycleT m = motorcycleNumberOfWheels m

-- TODO.
--  - discuss pro and cons of both solutions
--  - Reason about scaling the 2 approaches
--  - when the open world solution is required?

-- References
-- https://en.wikipedia.org/wiki/Expression_problem
-- https://www.youtube.com/watch?v=FWW87fvBKJg
