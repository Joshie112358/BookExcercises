module Cap13 where

import Control.Applicative
import Data.Char
import System.IO


newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
             [] -> []
             (x:xs) -> [(x,xs)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                   [] -> []
                   [(v,out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                    [] -> []
                    [(g,out)] -> parse (fmap g px) out)


instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                  [] -> []
                  [(v,out)] -> parse (f v) out)


-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [(v,out)] -> [(v,out)])


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit
lower :: Parser Char
lower = sat isLower
upper :: Parser Char
upper = sat isUpper
letter :: Parser Char
letter = sat isAlpha
alphanum :: Parser Char
alphanum = sat isAlphaNum
char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- some > 1
-- many >= 0

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)
         
space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident
natural :: Parser Int
natural = token nat
integer :: Parser Int
integer = token int
symbol :: String -> Parser String
symbol xs = token (string xs)

-- :: P (String -> [([Int],String)])
-- [(Int, String)]
nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

-- second grammar
-- expr ::= expr + expr | term
-- term ::= term * term | factor
-- factor ::= (expr) | nat
-- nat ::= 0 | 1 | 2 | ...

-- third grammar
-- expr ::= term + expr | term
-- term ::= factor * term | factor
-- factor ::= (expr) | nat
-- nat ::= 0 | 1 | 2 | ...
  
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             <|> do symbol "-"
                    e <- expr
                    return (t - e)
                    <|> return t
            
term :: Parser Int
term = do f <- expon
          do symbol "*"
             t <- term
             return (f * t)
             <|> do symbol "/"
                    t <- term
                    return (f `div` t)
                    <|> return f

expon :: Parser Int
expon = do c <- factor
           do symbol "^"
              e <- expon
              return (c^e)
              <|> return c
 

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> integer

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n,[])] -> n
            [(_,out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input"











-------------------------------------------------------------------------
-------------------------------------------------------------------------

-- Ejercicio 1
-- Parser () :: P (String -> [((), String)])
dropuntil :: (Char -> Bool) -> Parser ()
dropuntil p = do x <- item
                 if (p x)  then (return ()) else dropuntil p
                 <|> return ()

-- keepwhile :: (Char -> Bool) -> Parser String
-- keepwhile p = do x <- item
--                  if p x then
--                    do xs <- keepwhile p
--                       return (x:xs)
--                    else
--                       return (x:[])
--               <|> P (\inp -> [(inp,[])]) 


-- parse (dropuntil (== 'a')) "bcdabc"
-- parse (keepwhile (/= 'a')) "bcdef"

taketwo :: Parser String
taketwo =
  P (\inp -> if length inp >= 2 then [(((head inp):(inp !! 1):[]), inp)] else [([], inp)] ) 


comment' :: Parser String
comment' = do x <- taketwo
              if x == "--" then
                do dropuntil (== '\n')
                   z <- comment'
                   return (z)
              else
                do y <- item
                   z <- comment'
                   return (y : z)
           <|> do return []     

-- parse comment' "hola --comentario\n mundo --otro comentario\n jeje --bfhgbdjhg"
  
comment :: Parser ()
comment = do x <- comment'
             P (\inp -> [((), x)])

comment2 :: Parser ()
comment2 = do symbol "--"
              dropuntil (== '\n')
              return ()
-- parse (comment) "--dgufhd\n hola gfdg"


-- Ejercicio 5

data Op = Add | Sub | Mul | Div | Exp
  deriving (Eq, Ord, Read)
instance Show Op where
  show Add = "-+-"
  show Sub = "---"
  show Mul = "-*-"
  show Div = "-/-"
  show Exp = "-^-"

data Expr = Val Int | App Op Expr Expr
  deriving Read
instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

expr2 :: Parser Expr
expr2 = do t <- term2
           do symbol "+"
              e <- expr2
              return (App Add (t) (e))
              <|> return (t)
            
term2 :: Parser Expr
term2 = do f <- factor2
           do symbol "*"
              t <- term2
              return (App Mul (f) (t))
              <|> return (f)
            
factor2 :: Parser Expr
factor2 = do symbol "("
             e <- expr2
             symbol ")"
             return (e)
             <|> do x <- int
                    return (Val x)     

eval2 :: String -> Expr
eval2 xs = case (parse expr2 xs) of
            [(n,[])] -> n
            [(_,out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input Joshua"               

-- eval2 "2+3*5"


-- Ejercicio 6
-- Extension en las funciones expr, term, y factor
-- lineas 130 a 170



-- Ejercicio 7
-- Creacion funcion expon :: Parser Int que se llama en term y funciona como las
-- demas
-- ejemplo
-- eval "(1+2)^3*4"
-- 108


-- Ejercicio 8
{-
a)

expr ::= (e|expr -) factor
factor ::= (expr) | nat 
nat ::= 0 | 1 | ...

-}
-- b)

expr8' :: Parser Int
expr8' =
  do symbol "-"
     e <- factor8'
     do symbol "-"
        t <- factor8'
        let res = -e-t
        f <- expr8'
        return (res + f)
        <|> return (-e) 
  <|> do e <- factor8'
         do symbol "-"
            t <- factor8'
            let res = e-t
            f <- expr8'
            return (res + f)
            <|> return e
  <|> do return 0

factor8' :: Parser Int
factor8' = do symbol "("
              e <- expr8'
              symbol ")"
              return e
              <|> natural
{-

c)  una pequeña modificación no es suficiente, ya que compila pero no funciona,
como la primera instruccion de expr8 es recursiva,
el programa no pasa de ahi y solo se queda iterando hasta que ocurra um error.
Es por eso que se necesita otra forma de hacer las cosas.
No acepta dos signos menos consecutivos


-}

-- d)
-- expr ::= (e|expr -) factor
-- factor ::= (expr) | nat
-- nat ::= 0 | 1 | ...

expr8 :: Parser Int
expr8 =
  do s <- many (symbol "-")
     e <- factor8
     do s2 <- many (symbol "-")
        t <- factor8
        let res = e*((-1)^(length s))+t*((-1)^(length s2))
        f <- expr8
        return (res + f)
        <|> return (e*((-1)^(length s)))
  <|> return 0


factor8 :: Parser Int
factor8 = do symbol "("
             e <- expr8
             symbol ")"
             return e
             <|> natural


-- Ejercicio 9
-- Pequeña modificacion a la funcion evalc en la seccion de calculadora
-- El input 2+3*2(5 marcara la posicion aproximada del error pero (/) no



--------------------------------------------------------------------
-- calculadora
type Pos = (Int, Int)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | + |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"
       
showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
               process c xs
             else
               do beep
                  calc xs

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC" = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n" = evalc xs
             | elem c "cC" = clear
             | otherwise = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

evalc :: String -> IO ()
evalc xs = case parse expr xs of
            [(n,[])] -> calc (show n)
            [(n,ts)] -> do 
                           let pos = (length xs) - (length ts) + 1
                           goto (1,14)
                           beep
                           error ("ERROR: Invalid input at position " ++ (show pos))
            _ -> do goto (1,14)
                    beep
                    error "ERROR: Invalid input"        

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
