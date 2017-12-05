
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
isSublist list [] = False
isSublist (x:xs) (y:ys)
 | x == y = isMatch xs ys || isSublist ([x]++xs) ys
 | otherwise = isSublist ([x]++xs) ys

isMatch:: [Int] -> [Int] -> Bool
isMatch [] [] = True
isMatch [] list = True
isMatch list [] = False
isMatch (x:xs) (y:ys)
 |x == y = isMatch xs ys
 | otherwise = False

--test1 = isSublist [3, 4] [1, 2, 3, 4]
--test2 = isSublist [3, 4] [1, 3, 2, 4]
--test3 = isSublist [1, 2, 3] [1, 2, 3, 0, 1]
--test4 = isSublist [1, 2, 3] [1, 2, 2, 3, 1, 2]

combinator:: [a] -> [a] -> [[a]]
combinator [] list = []
combinator (x:xs) list = pair x list ++ combinator xs list

pair:: a -> [a] ->[[a]]
pair num [] = []
pair num (x:xs) = [[num] ++[x]] ++ pair num xs

--test1 = combinator [3,4] [1,2]
--test2 = combinator [] [3, 12,9]
--test3 = combinator [1,2] [1,3]
--test4 = combinator ["a","b"] ["c","d"]

mightOfPythagorus:: Int -> [(Int, Int, Int)]
mightOfPythagorus k= [(a,b,c)| c<-[1..k], b<-[1..c], a<-[1..b], (a^2) + (b^2) == (c^2)]
