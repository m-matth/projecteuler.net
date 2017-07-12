{-# LANGUAGE BangPatterns #-}

import Criterion.Main

------------------------------

pb001 n = sum([ x | x <- [1..n], x `mod` 3 == 0 || x `mod` 5 == 0 ])

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

-- Our benchmark harness.
main = defaultMain [
  bgroup "pb001" [
      bench "999"           $ whnf pb001 999
      , bench "999999"   $ whnf pb001 999999
               ],
    bgroup "pb002" [
      bench "ACCUMU/4000000"       $ whnf pb002 4000000
      , bench "ACCUMU/10e100000"   $ whnf pb002 (ceiling 10e100000::Integer)
      , bench "STRICT/4000000"     $ whnf pb002' 4000000
      , bench "STRICT/10e100000"   $ whnf pb002' (ceiling 10e100000::Integer)
      , bench "GKOENI/4000000"     $ whnf pb002'' 4000000
      , bench "GKOENI/10e100000"   $ whnf pb002'' (ceiling 10e100000::Integer)

      ]
  ]

