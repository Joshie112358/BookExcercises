module Cap11 where

import Data.Char
import Data.List
import System.IO
import System.Random hiding (next)
--import System.IO.Silently

size :: Int
size = 4

type Grid = [[Player]]

data Player = O | B | X
  deriving (Eq, Ord, Show, Read)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

winLen :: Int
winLen = size

isWinner :: Player -> [Player] -> Bool
isWinner p g | length g >= winLen && all (== p) (take winLen g) = True
             | length g < winLen = False
             | otherwise = isWinner p (tail g)
 

wins' :: Player -> Grid -> Bool
wins' p g = any (isWinner p) (rows ++ cols ++ dias)
  where
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

------------------------------------------------------
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = [" ","O"," "]
showPlayer B = [" "," "," "]
showPlayer X = [" ","X"," "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

---------------------------------------------------
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

------------------------------------------------------

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else
                     do putStr "ERROR: Invalid number"
                        getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise = do i <- getNat (prompt p)
                          case move g i p of
                            [] -> do putStrLn "ERROR: Invalid move"
                                     run' g p
                            [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

data Tree a = Node a [Tree a]
              deriving (Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g = []
          | full g = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 4

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
    where
      ts' = map minimax ts
      ps = [p | Node (_,p) _ <- ts']

-- -- d depth, O minimiza, X maximiza
-- minimax2 :: Int -> Tree Grid -> Tree (Grid, Int)
-- minimax2 d (Node g [])
--   | d == 0 || wins O g || wins X g || full g = Node (g, (boardScore g)) []
--   | d > 0 = undefined
-- minimax2 d (Node g ts)
--   | d == 0 || wins O g || wins X g || full g = Node (g, (boardScore g)) []
--   | d > 0 && turn g == O = Node (g, minimum values) cs
--   | d > 0 && turn g == X = Node (g, maximum values) cs
--     where
--       cs = map (minimax2 (d-1)) ts
--       values = [ x| Node (_,x) _ <- cs ]
      
minimax2 :: Tree Grid -> Tree (Grid, Int)
minimax2 (Node g [])
  | wins O g || wins X g || full g = Node (g, (boardScore g)) []
  | otherwise = Node (g, (boardScore g)) []
minimax2 (Node g ts)
  | wins O g || wins X g || full g = Node (g, (boardScore g)) []
  | turn g == O = Node (g, minimum values) cs
  | turn g == X = Node (g, maximum values) cs
    where
      cs = map minimax2 ts
      values = [ x| Node (_,x) _ <- cs ]

maxEval :: Int
maxEval = -2*size^2

minEval :: Int
minEval = 2*size^2

-- d depth a alpha b beta p player
-- abminimax :: Int -> Int -> Int -> Player ->Tree Grid -> (Tree (Grid, Int), Int, Int)
-- abminimax d alpha beta p (Node g ts)
--   | d == 0 || wins O g || wins X g || full g = Node (g, (boardScore g)) []
--   | p == X && beta <= alpha = Node (g, (boardScore g)) []
--   | p == X && beta > alpha
--     where
--       maxE = max (maxEval, eval)
--       abeval = map abminimax ((d-1), alpha2, beta, (next p)) ts
--       alpha2 = max (alpha, abeval)

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_,best) ts = minimax tree

bestmove' :: Grid -> Player -> Grid
bestmove' g p = head [x |(x,y) <- zip posible lens, y == m ]
  where
    posible = [g' | Node (g',p') _ <- ts, p' == best] 
    tree = prune depth (gametree g p)
    Node (_,best) ts = minimax tree
    lens = map count [Node (g',p') y | Node (g',p') y <- ts, p' == best]
    m = minimum lens

bestmove2' :: Grid -> Player -> Grid
bestmove2' g p = head [x |(x,y) <- zip posible lens, y == m ]
  where
    posible = [g' | Node (g',p') _ <- ts, p' == best] 
    tree = prune depth (gametree g p)
    Node (_,best) ts = minimax2 tree
    lens = map count [Node (g',p') y | Node (g',p') y <- ts, p' == best]
    m = minimum lens

shortbest :: Tree (Grid, Int) -> Player -> Grid
shortbest (Node (_, best) ts) p
  | p == O = head [g | Node (g,i) _ <- ts, i == minimum scores]
  | p == X = head [g | Node (g,i) _ <- ts, i == maximum scores]
  | p == B = empty
  where
    scores =  [i | Node (g,i) _ <- ts]

shortbest2 :: Tree (Grid, Int) -> Player -> [Grid]
shortbest2 (Node (_, best) ts) p
  | p == O = [g | Node (g,i) _ <- ts, i == minimum scores]
  | p == X = [g | Node (g,i) _ <- ts, i == maximum scores]
  | p == B = [empty]
  where
    scores =  [i | Node (g,i) _ <- ts]

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
boardScore :: Grid -> Int
boardScore g = scoreX - scoreO
  where
    scoreX = sum (map (unitScore X) (rows ++ cols ++ dias))
    scoreO = sum (map (unitScore O) (rows ++ cols ++ dias))
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

-- boardScore [[O,O,B],[O,B,B],[X,X,B]]

unitScore :: Player -> [Player] -> Int
unitScore p g = maximum (map (markCount p) (choping winLen g) )

markCount :: Player -> [Player] -> Int
markCount p xs | any (== (next p)) xs = 0
               | all (== p) xs = size^2
               | score == (length xs)-1 = 2*size
               | otherwise = score
                 where
                   score = length [x | x <- xs, x == p]
  
choping :: Int -> [a] -> [[a]]
choping n xs = if length xs >= winLen then take n xs : choping n (tail xs) else []
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
  
            
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          pturn <- selectP
          let tr = minimax2 $ prune depth (gametree empty O) 
          play tr empty O pturn
          return ()

selectP :: IO Player
selectP = do putStrLn "Select your turn: \nO is first\nX is second"
             p <- getLine
             let pturn = read p :: Player
             if pturn /= O && pturn /= X then
               do putStr "ERROR: Invalid character"
                  return O
             else
               do return pturn

play :: Tree (Grid, Int) -> Grid -> Player -> Player -> IO ()
play tr g p pturn = do cls
                       goto (1,1)
                       putGrid g
                       play' tr g p pturn

play' :: Tree (Grid, Int) -> Grid -> Player -> Player -> IO ()
play' (Node (grid,i) []) g p pturn
  | wins' O g = putStrLn "Player O wins!\n"
  | wins' X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  |otherwise = play' newtr g p pturn
  where
    newtr = minimax2 $ prune depth (gametree grid p) 
play' (Node (grid,i) tr) g p pturn
  | wins' O g = putStrLn "Player O wins!\n"
  | wins' X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == pturn = do i <- getNat (prompt p)
                    case move g i p of
                       [] -> do putStrLn "ERROR: Invalid move"
                                play' (Node (grid,i) tr) g p pturn
                       [g'] -> play ntr g' (next p) pturn
                                where
                                  ntr = head [Node (g,i) ts | Node (g,i) ts <- tr, g == g']
                                  
  | p == (next pturn) = do let t = "Player " ++ show (next pturn) ++ " is thinking..."
                           putStr t
                           
                           --2
                           -- let pos = (shortbest2 (Node (grid,i) tr) p)
                           -- r <- randomRIO (0, (length pos)-1)
                           -- let best = pos !! (r)
                           -- let ntr = head [Node (g,i) ts | Node (g,i) ts <- tr, g == best]
                           -- (play ntr $! (best)) (next p) pturn
                           
                           -- -- Programa original
                           --(play tr $! (bestmove2' g p)) (next p) pturn
                           
                           --4c
                           (play ntr $! (best)) (next p) pturn
                             where
                               best = shortbest (Node (grid,i) tr) p
                               ntr = head [Node (g,i) ts | Node (g,i) ts <- tr, g == best]
  




    

-------------------------------------------------------------------------------------------------------------------------
-- Ejercicios

-- Ejercicio 1
count :: Tree a -> Int
count (Node _ []) = 1
count (Node _ xs) = 1 + sum (map count xs)

--count (gametree (empty) O)
--549,946


-- Ejercicio 2
-- Modificacion en la funcion play' y creacion de la funcion shortbest2


-- Ejercicio 3
-- Creacion de la funcion bestmove' e implementarla en play' (ultima linea de la funcion)
-- los movimientos 0 8 2 demuestran que sirve la implementacion


-- Ejercicio 4
-- a) Creacion funcion selectP que se usa en main, modificacion a play y play' para que reciban
-- otro tipo Player que es el simbolo del jugador, para que asi el programa puede diferenciar
-- cual es la computadora y quien el jugador.


-- b) Creacion valor winLen y funciones isWinner wins'. wins' implementada en play'

-- c) Creacion scoreBoard, shortbest, modificacion play', play, main



-- let y = map (\x -> x/101) [0..101]
-- let z = sum [ (x^3)/(1-3*x+3*x^2) | x <- y ]
