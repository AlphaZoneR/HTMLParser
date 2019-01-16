import JSONParsing
import ComponentData
import Network.HTTP
import System.Environment

raw_data :: String
raw_data = "[{ \"angle\":-47.55226287542301, \"height\":338, \"id\":\"8201801f-a1b4-4581-bab7-833fafb0ae0a\", \"parent\":\"9a537c17-d114-4836-85ed-30a8543880ad\", \"width\":177, \"x\":412, \"y\":57 }, { \"angle\":29.23326155903709, \"height\":131, \"id\":\"56587b51-6596-47fe-8e8b-e58a389581ca\", \"parent\":\"9a537c17-d114-4836-85ed-30a8543880ad\", \"width\":110, \"x\":303, \"y\":69 }, { \"angle\":-0.8906270436127247, \"height\":70, \"id\":\"f6df69b0-217d-48ba-9fdc-dbf43d43f5b6\", \"parent\":\"9a537c17-d114-4836-85ed-30a8543880ad\", \"width\":61, \"x\":230, \"y\":72 }, { \"angle\":null, \"height\":65, \"id\":\"9a19f4b0-0256-4913-853a-d1251ea12850\", \"parent\":\"9a537c17-d114-4836-85ed-30a8543880ad\", \"width\":56, \"x\":149, \"y\":73 }, { \"angle\":null, \"height\":399, \"id\":\"9a537c17-d114-4836-85ed-30a8543880ad\", \"parent\":null, \"width\":503, \"x\":102, \"y\":20 }, { \"angle\":-26.111990892271052, \"height\":168, \"id\":\"7f324e6e-afe6-4271-ab1e-35825b100562\", \"parent\":\"9a537c17-d114-4836-85ed-30a8543880ad\", \"width\":110, \"x\":293, \"y\":213 }]"

header :: String
header = "import React, { Component } from 'react';\nimport Bhimg from './bhimg'\nimport Bhbtn from './bhbtn'\nimport Bhdiv from './bhdiv'\nimport Bhpar from './bhpar'\nimport Bhtxt from './bhtxt'\n\nclass App extends Component {\nrender() {\n    return (\n"

footer :: String
footer = ");\n  }\n}\n\nexport default App;\n"

parsed_data :: String -> [[(String, JValue)]]
parsed_data = unwrap . to_json 

ourTree :: String -> ComponentTree
ourTree = treeFromList Leaf . map (\a -> N (fromJObj a) []) . parsed_data

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
