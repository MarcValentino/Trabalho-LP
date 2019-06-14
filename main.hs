data Tree a = Operation a (Tree a) (Tree a) | Atom a | EmptyTree deriving (Show, Read, Eq)

--evaluate::(Eq a) => Tree a -> Bool

--evaluate (EmptyTree) = False
--evaluate (Atom a) = False
--evaluate (Operation a left right)
--    | a == ">" = ((evaluate left) == False) || ((evaluate right) == True)
--    | otherwise = False

--Acha todos os nós conectados a um determinado nó pela execução de um programa

lookUpPaths:: [(String, String)] -> String -> [String]
lookUpPaths [] _ = []
lookUpPaths (nd:list) path  
    |((snd nd) == path) = fst nd : lookUpPaths list path
    |((snd nd) /= path) = lookUpPaths list path
    |otherwise = []
    
        
findNode::[(String, [(String, String)])] -> [String] -> Int

findNode _ [] = -1
findNode graph nodes 
    | (fst $ head graph) == node = 0
    | otherwise = (findNode (tail graph) nodes) + 1
    where node = head nodes

graph = [("A", [("C", "a"), ("B", "a")]), ("B", [("D", "a"), ("E", "a")]), ("C", [("B", "a")]), ("D", []), ("E", [])]


--(Incompleto - Referente a SearchGraph e SearchGraphAux) 
--Pega todos os nós que podem ser acessados dada uma iteração de um programa

searchGraph::[(String, [(String, String)])] -> String ->  [[String]]

searchGraph graph program = [(fst $ head graph)] : searchGraphAux graph program (lookUpPaths (snd(head graph)) program)

searchGraphAux::[(String, [(String, String)])] -> String -> [String] -> [[String]]

searchGraphAux [] _ _= []
searchGraphAux _ _ []= []
searchGraphAux graph program (nd:nodes) = 
    let destination = findNode graph [nd]
        path = searchPath graph program destination
    in
        path : searchGraphAux graph program nodes

--Pega um e somente um caminho acessível pela iteração de um programa

searchPath::[(String, [(String, String)])] -> String -> Int -> [String]

searchPath [] _ _ = []
searchPath _ _ (-1) = [] 
searchPath graph path i = 
    let currNode = graph !! i
        connectedNodes = lookUpPaths (snd currNode) path
        nextNode = findNode graph connectedNodes
    in
        (fst currNode) : searchPath graph path nextNode        
        

