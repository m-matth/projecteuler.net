{-# LANGUAGE BangPatterns #-}

import Criterion.Main


import Data.Numbers.Primes -- pb003, pb007
import Data.Char (digitToInt)
import Data.List (transpose)

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
-- super-primorielle
pb005 n = foldr (\x y -> x `lcm` y) 1 [2..n]
------------------------------

pb006 n = (foldl (+) 0 [1..n])^2 - (foldl (+) 0 [x^2 | x <- [1..n]])
pb006' n = ((n^2 +n) `div` 2)^2 - ((n*(n+1)*(2*n+1))`div`6)


pb007 n = last(take n $ (wheelSieve 6 :: [Integer]))

pb008_data = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

digit_list = map digitToInt (show pb008_data)
pb008 = maximum [ foldr (*) (1) foo | foo <- (transpose [digit_list, digit_list, (tail((tail(digit_list)))), tail((tail((tail(digit_list)))))])]

buildProducts = transpose [digit_list, tail(digit_list), (tail((tail(digit_list)))), tail((tail((tail(digit_list)))))]

pb008' = maximum [ foldr (*) (1) foo | foo <- buildProducts ]


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
      ],
    bgroup "pb005" [
      bench "10"  $ whnf pb005 10,
      bench "20"  $ whnf pb005 20
      ]
  ]
