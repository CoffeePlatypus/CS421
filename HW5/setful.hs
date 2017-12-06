
type Set a = (a -> Bool)
--type Set a = [a]

setSuchThat ::(a -> Bool) -> (Set a)
setSuchThat funct = funct

unionSet:: Set a-> Set a-> Set a
unionSet f g = (\x-> f x || g x)

intersectSet:: Set a-> Set a-> Set a
intersectSet f g = (\x-> f x && g x)

memberSet :: Set a -> a -> Bool
memberSet function element = function element

complementSet :: Set a -> Set a
complementSet funct = (\x-> (not) (funct x))
