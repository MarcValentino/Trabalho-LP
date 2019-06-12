data Tree a = Operation a (Tree a) (Tree a) | Atom a | EmptyTree deriving (Show, Read, Eq)

--evaluate::(Eq a) => Tree a -> Bool

--evaluate (EmptyTree) = False
--evaluate (Atom a) = False
--evaluate (Operation a left right)
--    | a == ">" = ((evaluate left) == False) || ((evaluate right) == True)
--    | otherwise = False

lookUpPaths::(Eq a)=> [(a, a)] -> a -> [a]
lookUpPaths [] _ = []
lookUpPaths list path  
    |((snd first) == path) = fst first : lookUpPaths (tail list) path
    |((snd first) /= path) = lookUpPaths (tail list) path
    |otherwise = []
    where first = head list
        

searchGraph::(Eq a)=>[(a, [(a, a)])] -> a -> [a]

searchGraph [] _ = [] 
searchGraph graph path = 
    lookUpPaths (snd currNode) path
    where
        currNode = head graph
        

