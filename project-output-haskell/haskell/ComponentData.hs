module ComponentData (module ComponentData) where

import JSONData

data Component = Component { name   :: String
                           , hpos   :: Int
                           , vpos   :: Int
                           , width  :: Int
                           , height :: Int 
                           , nodeID :: String
                           , parentID :: String}
                           deriving Eq

data ComponentTree = Leaf | N Component [ComponentTree] deriving Eq

instance Show Component where
    show (Component name x y w h i p) = concat ["<",name
                                           , " x=\"", show x, "px\""
                                           , " y=\"", show y, "px\""
                                           , " width=\"", show w, "px\""
                                           , " height=\"", show h, "px\""
                                           , " skey=", show i, ">"
                                           ]

instance Show ComponentTree where
    show (N p xs)
        | xs == []  = concat [start p, "\t", end p, "\n"]
        | otherwise = concat [start p, "\n", iter xs, end p, "\n"]
            where
                start x = show x
                end   x = concat ["</", name x, ">"]
                iter []   = ""
                iter (x:ns) = concat [show x, iter ns]


sift :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
sift _ [] = ([], [])
sift p (x:xs)
    | p x       = (x:sat, nsat)
    | otherwise = (sat, x:nsat)
        where (sat, nsat) = sift p xs

isParent :: ComponentTree -> ComponentTree -> Bool
isParent Leaf (N c _) = [] == (parentID c)
isParent (N p _) (N c _) = (nodeID p) == (parentID c)

children :: ComponentTree -> [ComponentTree] -> [ComponentTree]
children tree = fst . sift (isParent tree) 

rest :: ComponentTree -> [ComponentTree] -> [ComponentTree]
rest     tree = snd . sift (isParent tree)

treeFromList :: ComponentTree -> [ComponentTree] -> ComponentTree
treeFromList tree [] = tree
treeFromList Leaf xs = treeFromList root (rest Leaf xs)
    where [root] = children Leaf $ xs
treeFromList root xs   = insert root (children root xs) (rest root xs)

insert :: ComponentTree -> [ComponentTree] -> [ComponentTree] -> ComponentTree
insert base [] [] = base
insert base [] rs = base
insert (N r ns) (x:xs) [] = insert (N r (x:ns)) xs []
insert (N r ns) (x:xs) rs = insert (N r (newTree:ns)) xs newRest
    where newTree = treeFromList x rs
          newRest = rest x rs
