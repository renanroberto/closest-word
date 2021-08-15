module Examples where

{-@ even :: {n:Int | n mod 2 = 0} @-}
even :: Int
even = 2


{-@ type BoundedInt M N = {x:Int | M <= x && x <= N} @-}

{-@ color :: (BoundedInt 0 255, BoundedInt 0 255, BoundedInt 0 255) @-}
color :: (Int, Int, Int)
color = (255, 0, 127)


{-@ nicknamer :: String -> {s:String | len s <= 2} @-}
nicknamer :: String -> String
nicknamer name = take 2 name
