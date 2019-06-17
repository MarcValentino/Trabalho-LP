data Tree a = Operation String (Tree String) (Tree String) | Atom String | EmptyTree deriving (Show, Read, Eq)

evaluate::Tree a -> Bool

testTree = Operation "<>" (Operation "^" (Atom "p") (Atom "q")) (Atom "q")

--Avalia sentenças básicas, no momento considerando p falso e q verdadeiro

evaluate (EmptyTree) = False
evaluate (Atom "p") = False
evaluate (Atom "q") = True
evaluate (Operation "~" leftTree (EmptyTree)) = not $ evaluate leftTree
evaluate (Operation "~" (EmptyTree) rightTree) = not $ evaluate rightTree
evaluate (Operation a left right)
    | a == ">" = (not (evaluate left)) || (evaluate right)
    | a == "^" = (evaluate left) && (evaluate right)
    | a == "v" = (evaluate left) || (evaluate right)
    | a == "<>" = (evaluate left) == (evaluate right)
    | otherwise = False