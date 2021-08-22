module Main where

import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Rando         (pickOne)


{-@ type SizedWord N = {s:String | len s = N}  @-}

{-@ type GameWord = SizedWord 5 @-}

{-@ isGameWord :: s:String -> {b:Bool | b <=> len s = 5} @-}
isGameWord :: String -> Bool
isGameWord w = length w == 5

{-@
filter' :: forall <p :: a -> Bool, w :: a -> Bool -> Bool>.
           {y :: a, b :: {v : Bool<w y> | v} |- {v:a | v == y} <: a<p>}
           (x:a -> Bool<w x>) -> [a] -> [a<p>]
@-}
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) =
  if p x then x : filter' p xs else filter' p xs

{-@ filterGameWords :: [String] -> [GameWord] @-}
filterGameWords :: [String] -> [String]
filterGameWords ws = filter' isGameWord ws


data Player = Player1 | Player2
  deriving (Eq, Show)


{-@
data GameState = GameState
  { turn     :: Nat
  , player   :: Player
  , p1Points :: Float
  , p2Points :: Float
  , allWords :: [GameWord]
  }
@-}

data GameState = GameState
  { turn     :: Int
  , player   :: Player
  , p1Points :: Float
  , p2Points :: Float
  , allWords :: [String]
  } deriving Show


nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

{-@ hamming :: s:String -> {t:String | len s = len t} -> Nat @-}
hamming :: String -> String -> Int
hamming xs ys = go 0 xs ys
  where go :: Int -> String -> String -> Int
        go acc [] [] = acc
        go acc (x:xs) (y:ys)
          | x == y = go acc xs ys
          | otherwise = go (acc + 1) xs ys


{-@ getScore :: [GameWord] -> GameWord -> {f:Float | f >= 0} @-}
getScore :: [String] -> String -> Float
getScore words word =
  words
  <&> (hamming word)
  & maximum
  & fromIntegral
  & (\n -> if n == 0 then 2 else 1 / n)


{-@ getWords :: Nat -> [GameWord] -> IO [GameWord] @-}
getWords :: Int -> [String] -> IO [String]
getWords n = sequence . replicate n . pickOne

game :: GameState -> IO ()
game gs =
  if (turn gs <= 6)
    then play gs
    else endgame gs

{-@ lazy play @-}
play :: GameState -> IO ()
play gs = do
  let wordsCount = turn gs + 6
  pickedWords <- getWords wordsCount (allWords gs)

  putStrLn ""
  putStr "turn: "
  print (turn gs)
  putStr "player: "
  print (player gs)
  putStrLn ""

  putStr "Turn words: "
  putStrLn . unwords $ pickedWords
  putStr "Choose a word: "
  playerWord <- getLine
  putStrLn ""

  let playerScore =
        if length playerWord == 5
          then getScore pickedWords playerWord
          else 0

  let gs' = gs
        { turn = (turn gs) + 1
        , p1Points =
          if (player gs == Player1)
            then (p1Points gs) + playerScore
            else p1Points gs
        , p2Points =
          if (player gs == Player2)
            then (p2Points gs) + playerScore
            else p2Points gs
        , player = nextPlayer (player gs)
        }

  putStr "score: "
  print playerScore

  game gs'

endgame :: GameState -> IO ()
endgame gs = do
  putStr "Player 1: "
  print (p1Points gs)

  putStr "Player 2: "
  print (p2Points gs)
  putStrLn ""

  case compare (p1Points gs) (p2Points gs) of
    LT -> putStrLn "Player 2 wins!!!"
    GT -> putStrLn "Player 1 wins!!!"
    EQ -> putStrLn "Draw..."


main :: IO ()
main = do
  ws <- lines <$> readFile "wordlist_5.txt"
  let ws' = filterGameWords ws

  let initialGame = GameState
        { turn = 1
        , player = Player1
        , p1Points = 0
        , p2Points = 0
        , allWords = ws'
        }

  game initialGame
