import System.Environment


pb001 = sum([ x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0 ])


evenFib maxVal = fLoop maxVal (0, 1, 0) where
     fLoop maxVal (prev, curr, evensum) = (if curr <= maxVal then fLoop maxVal (curr, newVal, sumIfEven) else (curr, newVal, sumIfEven))
       where
           newVal = prev+curr
           sumIfEven = (if even prev then evensum + prev else evensum)


last3 :: (a, b, c) -> c
last3 (_, _, x) = x

pb002 = last3(evenFib 4000000)


main :: IO ()
main = do
     args <- getArgs
     print (last3(evenFib (read $ head args :: Integer)))

