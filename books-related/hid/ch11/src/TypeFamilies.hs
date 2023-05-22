{-# LANGUAGE TypeFamilies #-}

-- Type families: allow to map types onto types

-- Type synonym families: don't create new types
-- Data families: allow defining new data type
-- Associated families: defined inside type classes

module TypeFamilies() where


-- OPEN Type synonym family

type family Simplify t

type instance Simplify Integer = Integer
type instance Simplify Int = Integer
type instance Simplify Double = Integer
type instance Simplify String = String
type instance Simplify Char = String
type instance Simplify Bool = String


-- We have to process the data also, we need a type class
class Simplifier t where
  simplify :: t -> Simplify t

instance Simplifier Integer where
  simplify = id

instance Simplifier Int where
  simplify = fromIntegral


instance Simplifier Double where
  simplify = round

instance Simplifier String where
  simplify = id

instance Simplifier Bool where
  simplify = show

instance Simplifier Char where
  simplify = (: "")


-- CLOSE Type synonym family
type family Widen a where
  Widen Bool = Int
  Widen Int = Integer
  Widen Char = String
  Widen t = String

class Widener a where
  widen :: a -> Widen a

instance Widener Bool where
  widen True  = 1
  widen False = 0

instance Widener Int where
  widen = fromIntegral

instance Widener Char where
  widen c = [c]

instance Widener Double where
  widen = show


-- Data families
data family XList a

newtype instance XList () = XListUnit Integer

data instance XList Bool = XBits Integer Integer



class XListable a where
  xempty :: XList a
  xconst :: a -> XList a -> XList a
  xheadMay :: XList a -> Maybe a

instance XListable () where
  xempty = XListUnit 0

  xconst _ (XListUnit n) = XListUnit $ n + 1
  xheadMay (XListUnit 0) = Nothing
  xheadMay _             = Just ()


instance XListable Bool where
  xempty = XBits 0 0
  xconst b (XBits bits n) = XBits (bits * 2 + if b then 1 else 0) (n + 1)
  xheadMay (XBits _ 0)    = Nothing
  xheadMay (XBits bits _) = Just $ mod bits 2 == 1


-- AssociatedFamilies

class Graph g where
  type Vertex g
  data Edge g
  src, tgt :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]


newtype EdgeList = EdgeList [Edge EdgeList]

instance Graph EdgeList where
  type Vertex EdgeList = Int
  data Edge EdgeList  = MkEdge1 (Vertex EdgeList) (Vertex EdgeList)


-- GADTs

-- stores arbitrary types
data Dyn = S String | C Char | B Bool
-- the data constructor are basically:
-- S :: String -> Dyn
-- C :: Char -> Dyn
-- B :: Bool -> Dyn

-- WE LOSE TYPE INFORMATION!

data DynValue a where
  S' :: String -> DynValue String
  C' :: Char -> DynValue Char
  B' :: Bool -> DynValue Bool

getValue :: DynValue a -> a
getValue (S' s) = s
getValue (C' c) = c
getValue (B' b) = b


printValue :: DynValue a -> IO ()
printValue (S' s) = print s
printValue (C' c) = print c
printValue (B' b) = print b


-- we cannot define a function, we cannod provide a value of DynVal a for all the a
-- f :: a -> DynValue a
-- f = undefined

-- We can use this technique
data WrappedDynValue where
  Wrap :: DynValue a -> WrappedDynValue


fromString :: String -> WrappedDynValue
fromString str
  | str `elem` ["yes", "y", "True"] = Wrap $ B' True
  | str `elem` ["no", "n", "False"] = Wrap $ B' False
  | length str == 1 = Wrap $ C' $ head str
  | otherwise = Wrap $ S' str


printWrappedDynValue :: WrappedDynValue -> IO ()
printWrappedDynValue (Wrap dv) = printValue dv


