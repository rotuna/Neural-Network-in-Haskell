module Functions where

dot ::(Floating a) => [a] -> [a] -> a
-- Should probably check both lists are of same length
dot x y = sum [a*b | (a,b) <- zip x y]

sigmoid ::(Floating a) => [a] -> [a] -> a
sigmoid inputs weights = 1/(1 + (2.71828)**(-1* ( dot inputs weights )))

relu ::(Floating a, Ord a) => [a] -> [a] -> a
relu inputs weights  
        | value > 0.0 = value
        | otherwise   = 0
        where value = dot inputs weights
