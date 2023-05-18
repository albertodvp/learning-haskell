{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds #-}
module Lib where

import           Data.Proxy

-- We have terms, types and kinds.

-- We classify terms by types.
f2c f = (f - 32 ) * 5/9
-- :t f2c
-- f2c :: Fractional a => a -> a

-- We classify types by kinds.
-- :k Double
-- Double :: *
-- :k Maybe
-- Maybe :: * -> *

-- We have `*` and `->` to distinguish between concrete types and type constructors
-- :set -XNoStarIsType


-- We can test the kind for `Maybe a` using `ExplicitForAll`


-- We classify kinds
-- import Data.Kind (Type, Constraint)


-- Terms have a type which should be a type of kind Type. There are types of other kinds (Maybe :: Type -> Type) and they don't have term belongin to the. Does if make sense to have a type of kind Type without any terms at all? It does, it can be used as phantom parameters. We use those parameters to deliver information with types, not terms


-- unit is a phantom parameter
-- we need GeneralizedNewtypeDeriving for this
newtype Temp unit = Temp Double deriving (Num, Fractional)

data F
data C

paperBurning :: Temp F
paperBurning = 451

absoluteZero :: Temp C
absoluteZero = -273.15

f2c' :: Temp F -> Temp C
f2c' (Temp f) = Temp $ (f - 32 ) * 5/9

class UnitName u where
  unitName :: Proxy u -> String


-- InstanceSigs allows us to put the signatures in the instances
instance UnitName C where
  unitName :: Proxy C -> String
  unitName _ = "C"

instance UnitName F where
  unitName :: Proxy F -> String
  unitName _ = "F"


instance UnitName u => UnitName (Temp u) where
  unitName :: Proxy (Temp u) -> String
  -- In order to use the u inside the method body
  unitName _ = unitName (Proxy :: Proxy u)


instance UnitName u => Show (Temp u)  where
  show :: Temp u -> String
  show (Temp x) = show x ++ " " ++ unitName (Proxy :: Proxy u)


unit :: forall u . UnitName u => Temp u -> String
unit _ = unitName (Proxy :: Proxy u)


instance UnitName Temp where
  unitName _ = "_unspecified unit_"


-- I can do this thanks to AllowAmbiguousTypes
class UnitName' u where
  unitName' :: String


instance UnitName' C where
  unitName' = "C"


instance UnitName' F where
  unitName' = "F"

instance UnitName' u => UnitName' (Temp u) where
  unitName' = unitName' @u



unit' :: forall u . UnitName' u => Temp u -> String
unit' _ = unitName' @u


-- You can put operators by adding the TypeOperators extension
data a + b = Inl a | Inr b deriving Show

data a * b = a :*: b deriving Show

infixl 6 +
infixl 7 *  

first :: a * b -> a
first (a :*: _) = a

second :: a * b -> b
second (_ :*: b) = b

x :: Int + Double
x = Inl 42

type T = Int + Bool * String
y :: T
y = Inl 42

y' :: T
y' = Inr $ True :*: "42"

-- (a + a * a) + a * a * a 
type Point a = a + a * a + a * a * a

zero1D :: Point Int
zero1D = Inl $ Inl 0

zero2D :: Point Int
zero2D = Inl (Inr (0 :*: 0))

zero3D :: Point Int
zero3D = Inr $ 0 :*: 0 :*: 0

-- With DataKinds, TempUnits is both:
--  * a data type with values F and C
--  * a data kind with types F and C (F,C are promoted to types)

-- So:
--  * type constructors become kind constructors
--  * data constructors become type constructors
data TempUnits = F' | C'


newtype Temp' (u :: TempUnits) = Temp' Double deriving (Num, Fractional)

paperBurning' :: Temp' F'
paperBurning' = 451

absoluteZero' :: Temp' C'
absoluteZero' = -273.15



