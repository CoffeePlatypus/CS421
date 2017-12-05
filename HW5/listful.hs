
seal:: Ord a => [a] -> [a] -> [a]
seal [] alist = alist
seal alist [] = alist
seal (x:xs) (y:ys)
	| x<y = seal xs ([y] ++ ys)
	| otherwise seal ([x] ++ xs) ys
