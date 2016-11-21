--import Debug.Trace

data DualNumber a = D a a

instance Show a => Show (DualNumber a) where
  show (D x x') = show (x, x')

instance Eq a => Eq (DualNumber a) where
  D x x' == D y y'     = x == y

instance Ord a => Ord (DualNumber a) where
  D x x' <= D y y'     = x <= y

instance Num a => Num (DualNumber a) where
  (D x x') + (D y y')  = D (x+y) (x'+y')
  (D x x') - (D y y')  = D (x-y) (x'-y')
  (D x x') * (D y y')  = D (x*y) (x*y'+y*x')
  negate (D x x')      = D (negate x) (negate x')
  abs (D x x')         = D (abs x) (abs x')
  signum (D x x')      = D (signum x) 0
  fromInteger n        = D (fromInteger n) 0  

instance Fractional a => Fractional (DualNumber a) where
  (D x x') / (D y y')  = D (x/y) (x'/y - x*y'/(y*y))
  recip (D x x')       = D (recip x) (-x'/(x*x))
  fromRational r       = D (fromRational r) 0

instance Floating a => Floating (DualNumber a) where
  pi                   = D pi 0
  exp (D x x')         = D (exp x) ((exp x)*x')
  log (D x x')         = D (log x) (x'/x)
  sqrt (D x x')        = D (sqrt x) (x'/(2*sqrt(x)))
  (D x x') ** (D y y') = D (x**y) (x'*y*x**(y-1) + y'*log(x)*x**y)
  logBase a x          = log x / log a
  sin (D x x')         = D (sin x) (x' * cos x)
  cos (D x x')         = D (cos x) (-x' * sin x)
  tan (D x x')         = D (tan x) (x' / (cos x)**2)
  asin (D x x')        = D (asin x) (x' / sqrt (1-x*x))
  acos (D x x')        = D (acos x) (-x' / sqrt (1-x*x))
  atan (D x x')        = D (atan x) (x' / (1+x*x))
  sinh (D x x')        = D (sinh x) (x' * cosh x)
  cosh (D x x')        = D (cosh x) (x' * sinh x)
  tanh (D x x')        = D (tanh x) (x' * (1 - (tanh x)**2))
  asinh (D x x')       = D (asinh x) (x'/sqrt (1+x*x))
  acosh (D x x')       = D (acosh x) (x'/sqrt (x*x - 1))
  atanh (D x x')       = D (atanh x) (x'/(1-x*x))

value (D x x') = x
deriv (D x x') = x'

-- example
  
f x = helper 100 x
  where helper 0 y = y
        helper n y = helper (n-1) $ sin (x+y)

main = mapM print $ map (\x-> f (D x 1.0)) $ take 5 [1..]
