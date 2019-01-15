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

