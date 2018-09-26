module Functions where

prodSum ::(Floating a) => [a] -> [a] -> a
prodSum xs xy = sum [a*b | (a,b) <- zip xs xy]

sigmoid ::(Floating a) => [a] -> [a] -> a
sigmoid inputs weights = 1/(1 + (2.71828)**(-1* ( prodSum inputs weights )))


relu ::(Floating a, Ord a) => [a] -> [a] -> a
relu inputs weights  
        | value > 0.0 = value
	| otherwise   = 0
        where value = prodSum inputs weights
