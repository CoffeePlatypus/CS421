type Node = Integer
type Edge = (Node, Node)
type Graph = [Edge]
type Path = [Node]

--nodes---------------------------------------------------
nodes:: Graph ->[Node]
nodes [] = []
nodes ((a,b) : [])
 | a == b = [a]
 |otherwise = [a]++[b]
nodes ((a,b): zs)
 | a == b = addCond1 a zs
 |otherwise = addCond2 a b zs

addCond1 a zs
 |contains (nodes zs) a = nodes zs
 |otherwise = [a] ++ nodes zs

addCond2 a b zs
 |contains (nodes zs) a && contains (nodes zs) b = nodes zs
 |contains (nodes zs) a = [b]++ nodes zs
 |contains (nodes zs) b = [a]++ nodes zs
 |otherwise = [a,b] ++ nodes zs

contains:: [Node] -> Node -> Bool
contains [] _ = False
contains (x:xs) z
 |x == z = True
 | otherwise = contains xs z

--neighbors------------------------------------------------
neighbors:: Node -> Graph -> [Node]
neighbors _ [] = []
neighbors n ((a,b): zs)
 | n == a = [b] ++ neighbors n zs
 |otherwise = neighbors n zs

--detach--------------------------------------------------
detach:: Node -> Graph -> Graph
detach n [] = []
detach n ((a,b): zs)
 | (n == a) || (n == b) = detach n zs
 | otherwise = [(a,b)]++ detach n zs

--paths--------------------------------------------------
--paths:: Node -> Node -> Graph -> [Path]
paths n1 n2 g = pathy (neighbors n1 g) n1 n2 g


pathy (x:xs) n1 n2 g
 | x == n2 = [n2]
 | otherwise = [x] ++ pathy (neighbors x g) n1 n2 g


--paths n1 n2 g
-- | n1 == n2 = [n2]
-- | otherwise = [n1] ++ pathafy(neighbors n1 g) n2 g

--pathafy [] _ _ = []
--pathafy (x:xs) n2 g = paths x n2 g

  --[[n1,x,n2]| x <- (neighbors n1 g)]
