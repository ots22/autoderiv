import Autoderiv

-- Perturbation confusion? 

-- Notice the use of 'lift' here
g x = x * (derivative (\y -> lift x + y) 1)

main = print $ derivative g 1.0
