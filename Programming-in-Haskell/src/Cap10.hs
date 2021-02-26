module Cap10 where

import System.IO
import Data.Char
-- Ejercicio 1

putStr2 :: String -> IO ()
putStr2 xs = sequence_ [putChar x | x <- xs]


-- Ejercicio 2
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [] = return ()
putBoard (x:xs) = do putBoard xs
                     putRow (length (x:xs)) x

putBoard2 :: Board -> IO ()
putBoard2 = putBoard . reverse


{-
1:
2:
3:
4:
5:

-}

-- Ejercicio 3

putBoard' :: Board -> IO ()
putBoard' xs = sequence_ [putRow row num | (row, num) <- zip [1..] xs]


-- Ejercicio 4

summing :: Int -> Int -> IO (Int)
summing 0 x = return (x)
summing n x = do  y <- getLine
                  let add = read y :: Int
                  summing (n-1) (x+add)


adder :: IO ()
adder = do  putStr "How many numbers? "
            n <- getLine
            let num = (read n :: Int)
            let t = 0
            sum <- summing num t
            putStr "The total is "
            putStrLn (show sum)
            return ()

-- Ejercicio 5


adder2 :: IO ()
adder2 = do putStr "How many number? "
            n <- getLine
            let num = read n :: Int
            a <- sequence [getLine | _ <- [1..num] ]
            let s = [ read n :: Int | n <- a]
            putStr "The total is "
            putStrLn (show (sum s))
            return ()



-- Ejercicio 6

getCh :: IO Char 
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine2 :: IO String
readLine2 = do
               x <- getChar 
               if x == '\n' then
                return []
               else
                if x == '\DEL' then
                  do putChar '\b'
                     xs <- readLine2
                     return xs
                else
                  do xs <- readLine2
                     return (x:xs)



