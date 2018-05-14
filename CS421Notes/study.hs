
mlength:: [a] -> Int
mlength [] = 0
mlength (x:xs) = 1+ (mlength xs)

reversey:: [a] -> [a]
reversey [] = []
reversey (x:xs) = reversey xs ++ [x]

memberberry:: (Eq a)=> [a] -> a -> Bool
memberberry [] _ = False
memberberry (x:xs) z
 | x == z = True
 | otherwise = memberberry xs z

coffeefilter:: [a] -> (a->Bool) -> [a]
coffeefilter [] _ = []
coffeefilter (x:xs) f
 | f x = [x] ++ coffeefilter xs f
 | otherwise = coffeefilter xs f

appender:: [a] -> [a] ->[a]
appender a b = a++b

loser:: [a] -> a
loser (x:[]) = x
loser (_:xs) = loser xs
