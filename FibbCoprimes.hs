module FibbCoprimes where

newtype Sequence = Sequence (Int, Int, Int) deriving (Show,Eq,Ord)

domSequence :: Int -> Int -> [Int]
domSequence 0 0 = [-1]
domSequence a 0 = []
domSequence 0 b = []
domSequence a b = 
  if a > b
     then a : domSequence b (remainder a b)
     else a : domSequence a (remainder b a)
  where 
  remainder m n = m - ((m `div` n) * n)

fibGenSeq :: Int -> Int -> [Int]
fibGenSeq a1 size = pseudoFibSeq (Sequence (a1, a1+1, 1)) size

pseudoFibSeq :: Sequence -> Int -> [Int]
pseudoFibSeq sequence n = map (pseudoFib sequence) [1..n]

pseudoFib :: Sequence -> Int -> Int
pseudoFib (Sequence (a1, a2, n)) 1 = a1
pseudoFib (Sequence (a1, a2, n)) 2 = a2
pseudoFib (Sequence (a1, a2, n)) i =
  pseudoFib (Sequence (a1, a2, n)) (i - 2) + n * pseudoFib (Sequence (a1, a2, n)) (i - 1)
