{-# LANGUAGE BangPatterns #-}

import Criterion.Main


import Data.Numbers.Primes -- pb003

------------------------------

pb001 n = sum([ x | x <- [1..n], x `mod` 3 == 0 || x `mod` 5 == 0 ])


sumVal r n = ((x * (x + 1) `div` 2) * r) where x = n `quot` r
pb001' n = (sumVal 3 n) + (sumVal 5 n) - (sumVal 15 n)
------------------------------

evenFib maxVal = fLoop maxVal (0, 1, 0) where
     fLoop maxVal (prev, curr, evensum) = (if curr <= maxVal then fLoop maxVal (curr, newVal, sumIfEven) else (curr, newVal, sumIfEven))
       where
           newVal = prev+curr
           sumIfEven = (if even prev then evensum + prev else evensum)

evenFib' maxVal = fLoop maxVal (0, 1, 0) where
     fLoop maxVal (prev, !curr, !evensum) = (if curr <= maxVal then fLoop maxVal (curr, newVal, sumIfEven) else (curr, newVal, sumIfEven))
       where
           newVal = prev+curr
           sumIfEven = (if even prev then evensum + prev else evensum)

last3 :: (a, b, c) -> c
last3 (_, _, x) = x

pb002 n = last3(evenFib n)

pb002' n = last3(evenFib' n)

fib :: [Integer]
fib = 1 : 2 : [x+y | (x,y)<-zip fib (tail fib)]

pb002'' n = (sum . filter even . takeWhile (<=(n))) fib

------------------------------

--findDiv' :: (Integral a) => a -> [a] -> a
findDiv' n [x] = (n, x)
findDiv' n (x:xs)
  | val /= 0 = findDiv' n xs
  | otherwise = (n, x)
  where val = n `rem` x

divTest number primeList = divLoop number (primeList, number, []) where
  divLoop number (a, b, c)
    = (if (b > 1) then
          divLoop number (a, b `div` divisor, c ++ [divisor])
       else
          (a, b, c)
      )
    where
      findDiv [x] = x
      findDiv (x:xs)
        | val /= 0 = findDiv xs
        | otherwise = x
        where val = b `rem` x
      divisor = findDiv primeList

pb003 :: Integer -> Integer
pb003 n = maximum(last3(divTest n (takeWhile (<=n) $ (wheelSieve 6 :: [Integer]))))
--((wheelSieve 6 :: [Int]) !! n)


------------------------------

pb004 n = maximum [ x * y | x <-[999, 999-1 .. 100], y<-[999, 999-1..x], show(x*y) == reverse(show(x*y))]
pb004_1000 n = maximum [ x * y | x <-[9999, 9999-1 .. 1000], y<-[9999, 9999-1..x], show(x*y) == reverse(show(x*y))]

pb004' n = maximum [ x * y | x <- [maxX, maxX-11 .. minX], y <- [maxY, maxY-1..x], show(x*y) == reverse(show(x*y))]
  where
    minY = 1 * 10^n
    maxY = (minY * 10) -1
    maxX = (maxY `quot` 11) * 11
    minX = minY

------------------------------


main = defaultMain [
  bgroup "pb001" [
      bench "999"           $ whnf pb001 999
      , bench "999999"      $ whnf pb001 999999
      , bench "999"         $ whnf pb001' 999
      , bench "999999"      $ whnf pb001' 999999
      ],
    bgroup "pb002" [
      bench "ACCUMU/4000000"       $ whnf pb002 4000000
      , bench "ACCUMU/10e100000"   $ whnf pb002 (ceiling 10e100000::Integer)
      , bench "STRICT/4000000"     $ whnf pb002' 4000000
      , bench "STRICT/10e100000"   $ whnf pb002' (ceiling 10e100000::Integer)
      , bench "GKOENI/4000000"     $ whnf pb002'' 4000000
      , bench "GKOENI/10e100000"   $ whnf pb002'' (ceiling 10e100000::Integer)
      ],
    bgroup "pb003" [
      bench "ACC/600851475143"  $ whnf pb002 600851475143
      ],
    bgroup "pb004" [
      bench "loop"  $ whnf pb004 2,
      bench "skip"  $ whnf pb004' 2
--      bench "loop"  $ whnf pb004_1000 2,
--      bench "skip"  $ whnf pb004' 3

      ]
  ]
