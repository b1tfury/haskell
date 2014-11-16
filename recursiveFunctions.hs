and1 :: [Bool] -> Bool
and1 [] = True
and1 (b:bs)
	| b==False = False
	| otherwise = and1 bs

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (xs :xss) = xs ++ concat1 xss

replicate1 :: Int -> a ->[a]
replicate1 0 _ = []
replicate1 n x = x:replicate1 (n-1) x

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x<=y then x : merge xs (y:ys) else y:merge (x:xs) ys
