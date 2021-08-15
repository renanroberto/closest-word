module NonTrivialExamples where


{-@ type Ordered a = [a]<{\x y -> x <= y}> @-}

{-@ mergesort :: [a] -> Ordered a @-}
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where n = length xs `div` 2
        left  = take n xs
        right = drop n xs

{-@ merge :: x:Ordered a -> y:Ordered a -> Ordered a / [len x + len y] @-}
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | x > y  = y : merge (x:xs) ys


{-@ avg :: {v:[Int] | len v > 0} -> Int @-}
avg :: [Int] -> Int
avg xs = sum xs `div` length xs
