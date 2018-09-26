module Layer where 

-- Rough Implementation of a fully connected layer

layer :: ([a] -> [a] -> a) -> a -> [a] -> [[a]] -> [a]
layer func num_neurons inputs weights = [func inputs weight | weight <- weights]
