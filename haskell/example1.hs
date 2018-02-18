import Autoderiv

f x = iter 100 x
  where iter 0 y = y
        iter n y = iter (n-1) $ sin (x+y)

main = mapM print $ map (derivativeAndValue f) $ take 5 [1..]
