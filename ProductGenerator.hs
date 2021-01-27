module ProductGenerator where

import Data.List

newtype DivisorAndFactors = DivisorAndFactors (Int, (Int, Int)) deriving (Eq,Ord)

instance Show DivisorAndFactors where
  show (DivisorAndFactors (divisor, (factor1, factor2))) =
    (show divisor) ++ " divides " ++ (show factor1) ++ "(" ++ (show factor2) ++ ")"

solutions :: [Int] -> [[DivisorAndFactors]]
solutions list =
  map (\x -> solutionSet x list) $ primeless list

solutionSet :: Int -> [Int] -> [DivisorAndFactors]
solutionSet a list =
  sort . orderSolutions . pruneSolutions . filter trueSol $ divisibleProducts a list
    where
    trueSol (DivisorAndFactors (i,(j,k))) = (i `divs` j == False) &&
                        (i `divs` k == False)

orderSolutions :: [DivisorAndFactors] -> [DivisorAndFactors]
orderSolutions [] = []
orderSolutions ((DivisorAndFactors (i,(j,k))) : xs) =
  if k < j
  then (DivisorAndFactors (i,(k,j))) : orderSolutions xs
  else (DivisorAndFactors (i,(j,k))) : orderSolutions xs

pruneSolutions :: [DivisorAndFactors] -> [DivisorAndFactors]
pruneSolutions []             = []
pruneSolutions ((DivisorAndFactors (i,(j,k))) : xs) =
  if j == k || not  ((DivisorAndFactors (i,(k,j))) `elem` xs)
  then (DivisorAndFactors (i,(j,k))) : pruneSolutions xs
  else pruneSolutions xs

divisibleProducts :: Int -> [Int] -> [DivisorAndFactors]
divisibleProducts a list =
  map (DivisorAndFactors) $ zip (cycle [a]) $ filter divsByA $ divHelper list
    where
    divsByA (m,n)    = a `divs` (m*n)
    divHelper list   = foldl (++) [] $ map constructTuple list
    constructTuple n = map (\x -> (n,x)) list

divs :: Int -> Int -> Bool
divs 0 0 = True
divs a b = b - (a * (b `div` a)) == 0 

primeless :: [Int] -> [Int]
primeless list =
  filter isntPrime list
    where
    isntPrime n = (n `elem` (1:primes)) == False
    primes = primeGen [2..100]

primeGen :: [Int] -> [Int]
primeGen [] = []
primeGen (x:xs) = x : (primeGen $ filter (\n -> x `divs` n == False) xs)
