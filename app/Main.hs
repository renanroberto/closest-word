module Main where

{-@ hamming :: s:String -> {r:String | len s = len r} -> Nat @-}
hamming :: String -> String -> Int
hamming xs ys = go 0 xs ys
  where go :: Int -> String -> String -> Int
        go acc [] [] = acc
        go acc (x:xs) (y:ys)
          | x == y = go acc xs ys
          | otherwise = go (acc + 1) xs ys

main :: IO ()
main = putStrLn "Hello"
