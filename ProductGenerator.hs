module ProductGenerator where

import Data.List

solutions :: [Int] -> [[(Int, (Int,Int))]]
solutions list =
  map (\x -> solutionSet x list) $ primeless list

solutionSet :: Int -> [Int] -> [(Int,(Int,Int))]
solutionSet a list =
  sort $ orderSolutions $ pruneSolutions $ filter trueSol $ divisibleProducts a list
    where
    trueSol (i,(j,k)) = (i `divs` j == False) &&
                        (i `divs` k == False)

orderSolutions :: [(Int,(Int,Int))] -> [(Int,(Int,Int))]
orderSolutions [] = []
orderSolutions ((i,(j,k)):xs) =
  if k < j
  then (i,(k,j)) : orderSolutions xs
  else (i,(j,k)) : orderSolutions xs

pruneSolutions :: [(Int,(Int,Int))] -> [(Int,(Int,Int))]
pruneSolutions []             = []
pruneSolutions ((i,(j,k)):xs) =
  if j == k || not  ((i,(k,j)) `elem` xs)
  then (i,(j,k)) : pruneSolutions xs
  else pruneSolutions xs

divisibleProducts :: Int -> [Int] -> [(Int,(Int,Int))]
divisibleProducts a list =
  zip (cycle [a]) $ filter divsByA $ divHelper list
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

products :: [Int] -> [Int]
products list = 
  sort $ removeDups $ productHelper list
    where
    productHelper list = foldl (++) [] $ map multiplyByN list
    multiplyByN n      = map (n *) list

removeDups :: [Int] -> [Int]
removeDups [] = []
removeDups (x:xs) =
  if x `elem` xs
  then removeDups xs
  else x:removeDups xs
