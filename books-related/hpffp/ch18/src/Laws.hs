module Laws where

-- 1 - Identity

-- right identity
--m >>= return = m

-- left identity
--return x >>= f = f x



-- 2 - Associativity

--m >>= f >>= g = m >>= (\x -> f x >>= g)
