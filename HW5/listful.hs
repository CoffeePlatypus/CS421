
seal:: Ord a => [a] -> [a] -> [a]
seal [] alist = alist
seal alist [] = alist
seal (x:xs) (y:ys)
 | x<y = [x] ++ seal xs ([y] ++ ys)
 | otherwise = [y] ++ seal ([x] ++ xs) ys


--how do I test this?
--main =
--test1 = seal [3, 6, 7] [1, 2]
--test2 = seal [] [3, 12]
--test3 = seal [1, 2, 3] [4, 5, 6]
--test4 = seal ["a", "d"] ["b"]

isSublist:: [Int] -> [Int] -> Bool
-- sublist -> list
isSublist list [] = False
isSublist (x:xs) (y:ys)
 | x == y = isMatch xs ys || isSublist ([x]++xs) ys
 | otherwise = isSublist ([x]++xs) ys

isMatch:: [Int] -> [Int] -> Bool
isMatch [] [] = True
isMatch [] list = False
isMatch list [] = False
isMatch (x:xs) (y:ys)
 |x == y = isMatch xs ys
 | otherwise = False

test1 = isSublist [3, 4] [1, 2, 3, 4]
test2 = isSublist [3, 4] [1, 3, 2, 4]
test3 = isSublist [1, 2, 3] [1, 2, 3, 0, 1]
test4 = isSublist [1, 2, 3] [1, 2, 2, 3, 1, 2]
