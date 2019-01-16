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
