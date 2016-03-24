-- 参考:http://shin.hateblo.jp/entries/2012/06/25
--1--
--sum [n2 |n <- [1..100]]

--2--
replicate2 :: Int -> a -> [a]
replicate2 n a =  [a | _ <- [0..n -1 ] ]

--3--
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- l, y <- l , z <- l, x^2 + y^2 == z^2]
          where l = [1..n]

--4--
factors :: Int -> [Int]
factors n = [x | x <-[1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum(init(factors x)) == x ]
--initの解説まだでてきてないけどいいか

--5--
--concat [[(x,y)|y<-[4,5,6] ] |x<-[1,2,3]]
--正直挙動良くわかってない

--6--
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <-t, k == k']

positions :: Eq a => a  -> [a] -> [Int]
positions x xs  = find  x ( zip xs [0 ..n]) where n = length xs -1

--7--
scolarproduct :: [Int] -> [Int] -> Int
scolarproduct xs ys = sum [ x *y  | (x,y) <- zip xs ys ]
