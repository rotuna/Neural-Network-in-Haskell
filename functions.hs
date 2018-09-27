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

-- Loss Functions 

mse ::(Floating a) => [a] -> [a] -> a
mse x y
        | length x == length y = let len = length x
                                     total = sum [(xi - yi)**2 | (xi, yi) <- zip x y]
                                 in total/(fromIntegral len)
        | otherwise = error "Invalid Inputs: Lists of unequal lengths"


l1_loss ::(Floating a) => [a] -> [a] -> a
l1_loss x y
        | length x == length y = let len = length x
                                     total = sum [abs (xi - yi) | (xi, yi) <- zip x y]
                                 in total/(fromIntegral len)
        | otherwise = error "Invalid Inputs: Lists of unequal lengths"
