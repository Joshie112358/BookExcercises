module Snake where

import Control.Applicative
import Data.Char
import System.IO
import System.Random

type Pos = (Int, Int)
type Snake = [Pos]

data Mov = U | D | L | R | N
  deriving (Eq, Ord, Show, Read)

width :: Int
width = 20
height :: Int
height = 20


-- move :: Snake -> Snake
-- move Tail =
-- move Body (x,y) Tail = Tail
-- move Body (x,y) xs = Body (x,y) (move xs)

direc :: Mov -> Pos -> Pos
direc U (x,y) = (x, y-1 `mod` height)
direc D (x,y) = (x, y+1 `mod` height)
direc L (x,y) = (x-1 `mod` width, y)
direc R (x,y) = (x+1 `mod` width, y)
direc N (x,y) = (x,y)

move :: Pos -> Mov -> Snake -> Snake
move food d xs = if nx == food then [food] ++ xs else [nx] ++ (init xs)
  where
    nx = direc d (last xs) 

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return (x)

getMove :: IO Mov
getMove = do inp <- getCh
             if elem inp "WwAaSsDd" then
               case toUpper inp of
                 'W' -> return U
                 'S' -> return D
                 'A' -> return L
                 'D' -> return R
             else
               return N
          <|> return N

putBoard :: IO ()
putBoard = do sequence [ do {goto p; putChar '-'} | p <- zip [0..width] (repeat height) ]
              sequence [ do {goto p; putChar '|'} | p <- zip (repeat width) [0..height] ]
              return ()
  -- where
  --   p = zip [0..width] (repeat height)

printSnake :: Snake -> IO ()
printSnake [] = return ()
printSnake (p:ps) = do goto p
                       putChar (chr 219)
                       printSnake ps
                       
putFood :: Snake -> IO Pos
putFood s = do x <- randomRIO (0, width)
               y <- randomRIO (0, height)
               if null [p | p <- s, p == (x,y) ] then
                 return (x,y)
               else
                putFood s

printFood :: Pos -> IO ()
printFood p = do goto p
                 putChar (chr 64)

main :: IO ()
main = do cls
          play [(5,5)] (10,10) D
          return ()
          
          

play :: Snake -> Pos -> Mov -> IO () 
play snake food dir =
  do direc <- (getMove)
     cls
     putBoard
     printSnake snake
     printFood food
     let nsnake = move food direc snake
     if elem (head nsnake) snake && length nsnake >= 3 then
       do goto (0, height+1)
          putStr "Game Over"
          return ()
     else
      do
       wait 500000
       if length nsnake > length snake then    --comio
         do nfood <- putFood snake
            play nsnake nfood dir
       else                                   -- no comio
         play nsnake food dir
       
          
       
wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]


