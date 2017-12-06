
seal:: Ord a => [a] -> [a] -> [a]
seal [] alist = alist
seal alist [] = alist
seal (x:xs) (y:ys)
 | x<y = [x] ++ seal xs ([y] ++ ys)
 | otherwise = [y] ++ seal ([x] ++ xs) ys

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

combinator:: [a] -> [a] -> [[a]]
combinator [] list = []
combinator (x:xs) list = pair x list ++ combinator xs list

pair:: a -> [a] ->[[a]]
pair num [] = []
pair num (x:xs) = [[num] ++[x]] ++ pair num xs

mightOfPythagorus:: Int -> [(Int, Int, Int)]
mightOfPythagorus k= [(a,b,c)| c<-[1..k], b<-[1..c], a<-[1..b], (a^2) + (b^2) == (c^2)]
