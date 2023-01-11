data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Price = Price Int deriving (Eq, Show)
data Size = Size Int deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 200)


-- Exercises

isCar :: Vehicle -> Bool
isCar (Car _ _)   = True
isCar (Plane _ _) = False


isPlane :: Vehicle -> Bool
isPlane = not . isCar


areCars :: [Vehicle] -> [Bool]
areCars = map isCar


getManu :: Vehicle -> Manufacturer
getManu v = if isCar v then m else error "Vehicle is not a Car"
  where (Car m _) = v

getManu' :: Vehicle -> Manufacturer
getManu' (Car m _)   = m
getManu' (Plane _ _) = error "Vehicle is not a Car"
