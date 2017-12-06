
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

test1 = memberSet (setSuchThat (\x -> x == "coke")) "coke"
test2 = memberSet (setSuchThat (\x -> x == "coke")) "pepsi"
test3 = memberSet (complementSet (setSuchThat(\ x -> x == "coke"))) "coke"
test4 = memberSet (unionSet (setSuchThat (\x -> x == "coke"))   (setSuchThat (\x -> x == "pepsi"))) "pepsi"
test5 = memberSet (unionSet (setSuchThat(\ x -> x == "coke"))  (setSuchThat(\ x -> x == "pepsi"))) "coke"
test6 = memberSet (unionSet (setSuchThat(\ x -> x == "coke"))   (setSuchThat(\ x -> x == "pepsi"))) "sprite"
test7 = memberSet (intersectSet (setSuchThat(\ x -> x == "coke"))   (setSuchThat(\ x -> x == "pepsi"))) "coke"
test8 = memberSet (complementSet(setSuchThat(\x -> x > 0))) 5 
