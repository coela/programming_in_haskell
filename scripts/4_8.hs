halve :: [a] -> ([a],[a])
halve ns =  splitAt  ( (length ns) `div` 2) ns

--a--
safetail :: [a] -> [a]
safetail as = if null as then [] 
							else tail as
--b--
safetail2 :: [a] -> [a]
safetail2 as | null as = []
						 | otherwise = tail as
--c--
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 as = tail as

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
