module JSONData (module JSONData) where

data JSON    = JSON JCollector
type JArray  = [JValue]
type JObject = [(String, JValue)]

data JCollector = JObj JObject
                | JArr JArray

data JValue = JString   String
            | JInt      Int 
            | JDouble   Double
            | JCollect  JCollector
            | JBool     Bool
            | JNull

instance Show JSON where
    show (JSON collector) = show collector

instance Show JValue where
    show (JString str) = show str
    show (JInt nat) = show nat
    show (JDouble dbl) = show dbl
    show (JCollect col) = show col
    show (JBool bol) = show bol
    show JNull      = "null"

instance Show JCollector where
    show (JObj obj) = unlines . map show $ obj
    show (JArr arr) = show arr

jsonhead (JSON a) = jcollhead a
jcollhead (JObj a) = head a

module JSONParsing (module JSONParsing, module JSONData, module Parsing) where

import JSONData
import Parsing
import Data.Char

-- helper functions

validChar :: Char -> Bool
validChar c
    | c == '\"' = False
    | c == '\\' = False
    | c == '\b' = False
    | c == '\f' = False
    | c == '\n' = False
    | c == '\r' = False
    | c == '\t' = False
    | otherwise = True    

nonz :: Char -> Bool
nonz a = (isDigit a) && (a /= '0')

startDigit :: Parser Char
startDigit = sat nonz

getE :: Parser Char
getE = do char 'e'
        <|> char 'E'

getS :: Parser Char
getS = do char '+'
        <|> char '-'

-- simple value parsers        

jnat :: Parser Int
jnat = do x  <- startDigit
          xs <- many digit
          return $ read (x:xs)
        <|> do x <- digit
               return (read [x])

jint :: Parser Int
jint = do char '-'
          n <- jnat
          return (-n)
        <|> jnat

double :: Parser Double
double = do p <- fracPref
            i <- fracInfi
            t <- fracPost
            return $ read . concat $ [p,i,t]
          <|> do p <- fracPref
                 i <- fracInfi
                 return $ read (p++i)
          <|> do p <- fracPref
                 t <- fracPost
                 return $ read (p++t)
      where
        fracPref :: Parser String
        fracPref = do string "-0"
                    <|> string "0"
                    <|> do x   <- char '-'
                           xs  <- startDigit
                           xss <- many digit
                           return (x:xs:xss)
                    <|> do x   <- startDigit
                           xs  <- many digit
                           return (x:xs)

        fracInfi :: Parser String
        fracInfi = do dot  <- char '.'
                      digs <- some digit
                      return (dot:digs)

        fracPost :: Parser String
        fracPost = do e <- getE
                      d <- some digit
                      return (e:d)
                    <|> do e <- getE
                           s <- getS
                           d <- some digit
                           return (e:s:d)
    
bool :: Parser Bool
bool = do string "true"
          return True
        <|> do string "false"
               return False

-- simple JValue parsers

jkey :: Parser String
jkey = do space
          char '\"'
          v <- many . sat $ validChar
          char '\"'
          return v

jstring :: Parser JValue
jstring = do v <- jkey
             return (JString v)

jinteger :: Parser JValue
jinteger = do v <- jint
              return (JInt v)

jdouble :: Parser JValue
jdouble = do d <- double
             return (JDouble d)

jbool :: Parser JValue
jbool = do b <- bool
           return (JBool b)

jnull :: Parser JValue
jnull = do string "null"
           return JNull

-- atoms
jvalue :: Parser JValue
jvalue = do jstring
          <|> jdouble
          <|> jinteger
          <|> jbool
          <|> jnull
          <|> jcollect

jtouple :: Parser (String, JValue)
jtouple = do key <- jkey
             token (char ':')
             val <- jvalue
             return (key, val)

-- JCollector parsers

jvalueCollected :: Parser JValue
jvalueCollected = do val <- jvalue
                     token (char ',')
                     return val
                   <|> jvalue

jtoupleCollected :: Parser (String, JValue)
jtoupleCollected = do val <- jtouple
                      token (char ',')
                      return val
                    <|> jtouple

jarray :: Parser JCollector
jarray = do token (char '[')
            array <- many jvalueCollected
            token (char ']')
            return (JArr array)

jobject :: Parser JCollector
jobject = do token (char '{')
             object <- many jtoupleCollected
             token (char '}')
             return (JObj object)

jcollect :: Parser JValue
jcollect = do v <- jarray
              return (JCollect v)
            <|> do v <- jobject
                   return (JCollect v)

json :: Parser JSON
json = do v <- jarray
          return (JSON v)
        <|> do v <- jobject
               return (JSON v)

-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                        []     -> []
                        (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                        []        -> []
                        [(v,out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                        []        -> []
                        [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                        []        -> []
                        [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                        []        -> parse q inp
                        [(v,out)] -> [(v,out)])

-- Derived primitives

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
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat
       <|> (char '+' >> nat)

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

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

import JSONParsing
import ComponentData
import Network.HTTP
import System.Environment

header :: String
header = "import React, { Component } from 'react';\nimport Bhimg from './bhimg'\nimport Bhbtn from './bhbtn'\nimport Bhdiv from './bhdiv'\nimport Bhpar from './bhpar'\nimport Bhtxt from './bhtxt'\n\nclass App extends Component {\nrender() {\n    return (\n"

footer :: String
footer = ");\n  }\n}\n\nexport default App;\n"

parsed_data :: String -> [[(String, JValue)]]
parsed_data = unwrap . to_json 

ourTree :: String -> ComponentTree
ourTree xs = treeFromList Leaf $ map (\a -> N (fromJObj a) []) $ parsed_data xs

to_json :: String -> JSON
to_json xs = fst . head $ parse json xs

unwrap :: JSON -> [[(String, JValue)]]
unwrap = map (\x -> jjobj . object $ x) . array . jason
    where
        jason   :: JSON       -> JCollector
        array   :: JCollector -> [JValue]
        object  :: JValue     -> JCollector
        jjobj   :: JCollector -> [(String, JValue)]

        jason  (JSON j)     = j
        array  (JArr a)     = a
        array  _            = []
        object (JCollect o) = o
        object _            = JObj []
        jjobj  (JObj o)     = o
        jjobj  _            = []

keyMatch :: Eq a => a -> (a, b) -> Bool
keyMatch xs = (== xs) . fst

fromJObj :: [(String, JValue)] -> Component
fromJObj = get (Component "" 0 0 0 0 "" "") 
     where get   :: Component -> [(String, JValue)] -> Component
           range :: Double -> Double -> Bool
           c0    :: JValue -> String
           c1    :: JValue -> Int
           c2    :: JValue -> String

           range r a = ((r - 22.5) <= a) && (a <= (r + 22.5))

           c0 (JString str) = str
           c0  JNull        = []
           c1 (JInt    int) = int
           c2  JNull        = "Bhdiv"
           c2 (JDouble a)
               | range    0  a = "Bhpar"
               | range   45  a = "Bhtxt"
               | range   90  a = "Bhbtn"
               | range (-45) a = "Bhimg"

           get comp [] = comp
           get (Component on ox oy ow oh oi op) (x:xs)
               | keyMatch "id"     x = get (Component on ox oy ow oh  i op) xs
               | keyMatch "parent" x = get (Component on ox oy ow oh oi  p) xs
               | keyMatch "angle"  x = get (Component  n ox oy ow oh oi op) xs
               | keyMatch "x"      x = get (Component on  z oy ow oh oi op) xs
               | keyMatch "y"      x = get (Component on ox  y ow oh oi op) xs
               | keyMatch "width"  x = get (Component on ox oy  w oh oi op) xs
               | keyMatch "height" x = get (Component on ox oy ow  h oi op) xs
                   where i = c0 $ snd x 
                         p = c0 $ snd x
                         z = c1 $ snd x
                         y = c1 $ snd x
                         w = c1 $ snd x
                         h = c1 $ snd x
                         n = c2 $ snd x

--jsonURL = "http://127.0.0.1:8080/"
--192.168.0.35:8080

geturl :: IO String
geturl = do x <- getArgs
            return (if (x == [])
                        then "http://127.0.0.1:8080/"
                        else head x)


ioComponentTree :: IO ComponentTree
ioComponentTree = do jsonURL <- geturl
                     x <- simpleHTTP (getRequest jsonURL) >>= fmap (take 2048) . getResponseBody
                     return (ourTree x)


main :: IO ()
main = do x <- ioComponentTree
          putStr header
          print x
          putStr footer
                                                                                                                                fetcher.sh                                                                                          0000755 0001750 0001750 00000000135 13417202571 012772  0                                                                                                    ustar   bhzsolt                         bhzsolt                                                                                                                                                                                                                #!/bin/bash

while true
do
	clear
	./parseJSONtoTree "$1" > App.js
	cat App.js
	sleep 5
done
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   