module Main where

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Small

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Size
  = Small
  | Medium
  | Large
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m _) = Just m
getManu _ = Nothing

main :: IO ()
main = do
  print $ isCar myCar
  print $ isCar doge
  print $ isPlane myCar
  print $ isPlane doge
  print $ areCars [myCar, urCar, clownCar, doge]
  print $ getManu myCar
  print $ getManu doge
