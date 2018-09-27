module Functions where

-- Constants
e::(Floating a) => a
e = 2.71828

dot ::(Floating a) => [a] -> [a] -> a
-- Function to to get dot product of two lists
-- Throws an error is the lists are not equal length
dot x y 
        | length x == length y = sum [a*b | (a,b) <- zip x y]
        | otherwise            = error "Invalid Inputs: Lists of unequal length"

sigmoid ::(Floating a) => [a] -> [a] -> a
sigmoid inputs weights = 1/(1 + (e)**(-1* ( dot inputs weights )))

relu ::(Floating a, Ord a) => [a] -> [a] -> a
relu inputs weights  
        | value > 0.0 = value
        | otherwise   = 0
        where value = dot inputs weights

softmax ::(Floating a) => [a] -> [a]
softmax logits = 
        let exponents = [(e)**x | x <- logits]
            exponents_sum = sum exponents
        in [exponent/exponents_sum | exponent <- exponents]
