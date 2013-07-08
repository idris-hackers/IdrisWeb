module Main
-- Simple binding to a simple C random number library

%link C "rand_c.o"
%include C "rand_c.h"
-- %lib C "rand_c"

getRandom : Int -> Int -> IO Int
getRandom min max = mkForeign (FFun "random_number" [FInt, FInt] FInt) min max

-- Quick test harness
main : IO ()
main = do
  rand1 <- getRandom 100 1005
  rand2 <- getRandom 1200000 1230423
  putStrLn $ "random 1: " ++ (show rand1) ++ ", random 2: " ++ (show rand2)
