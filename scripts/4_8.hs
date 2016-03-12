--1--
halve :: [a] -> ([a],[a])
halve ns =  splitAt  ( (length ns) `div` 2) ns

--2--
safetail :: [a] -> [a]
safetail as = if null as then [] 
							else tail as
safetail2 :: [a] -> [a]
safetail2 as | null as = []
						 | otherwise = tail as
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 as = tail as

--3--
oror :: Bool -> Bool -> Bool
True  `oror` True   = True
True  `oror` False  = True
False `oror` True   = True
False `oror` False  = False

oror2 :: Bool -> Bool -> Bool
False `oror2` False = False
_ `oror2` _ = True

oror3 :: Bool -> Bool -> Bool
True `oror3` b = True
False `oror3` b = b

oror4 :: Bool -> Bool -> Bool
a `oror4` b | a == b = b
						| otherwise = True

--4--
and2 :: Bool -> Bool -> Bool
and2 a b = if  a then
					   if b then
						   True
						 else
						   False
					 else
					   False

--5--
and3 :: Bool -> Bool -> Bool
and3 a b = if a  then b
					 else False

--6--
mult = (\x -> (\y -> (\z -> x * y * z) ) )
