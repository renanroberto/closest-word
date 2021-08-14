module Main where

import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Rando         (pickOne)


{-@ hamming :: s:String -> {t:String | len s = len t} -> Nat @-}
hamming :: String -> String -> Int
hamming xs ys = go 0 xs ys
  where go :: Int -> String -> String -> Int
        go acc [] [] = acc
        go acc (x:xs) (y:ys)
          | x == y = go acc xs ys
          | otherwise = go (acc + 1) xs ys

{-@ type SWord N = {s:String | len s = N} @-}

{-@ getScore :: [SWord 5] -> SWord 5 -> Float @-}
getScore :: [String] -> String -> Float
getScore words word =
  words
  <&> (hamming word)
  & maximum
  & fromIntegral
  & (\n -> if n == 0 then 2 else 1 / n)


{-@ getWords :: Nat -> [String] -> IO [String] @-}
getWords :: Int -> [String] -> IO [String]
getWords n = sequence . replicate n . pickOne


data Player = Player1 | Player2
  deriving Show

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

data GameState = GameState
  { turn     :: Int
  , player   :: Player
  , p1Points :: Float
  , p2Points :: Float
  , allWords :: [String]
  } deriving Show

{-@ ignore game @-}
game :: GameState -> IO GameState
game gs = do
  pickedWords <- getWords ((turn gs) + 1) (allWords gs)

  putStr "Turn words: "
  putStrLn . unwords $ pickedWords
  putStrLn "Choose a word"
  playerWord <- getLine
  let playerScore = getScore pickedWords playerWord

  let gs' = gs
        { turn = (turn gs) + 1
        , p1Points = (p1Points gs) + playerScore
        , player = Player2
        }

  putStrLn "--- DEBUG ---"

  putStr "score: "
  print playerScore
  putStrLn ""

  putStrLn "game state:"
  print ("turn", (turn gs'))
  print ("player", (player gs'))
  print ("player1 points", (p1Points gs'))
  print ("player2 points", (p2Points gs'))
  putStrLn ""

  putStrLn "--- DEBUG ---"

  return gs'


main :: IO ()
main = do
  ws <- lines <$> readFile "wordlist_5.txt"

  let initialGame = GameState
        { turn = 1
        , player = Player1
        , p1Points = 0
        , p2Points = 0
        , allWords = ws
        }

  game initialGame
  putStrLn "end"
