module Interpolation (lagrange, lagrangeList, newton, newtonList) where

-- Lagrange interpolation formula for a single point
-- PointToPredict x -> [(xi, yi)] -> PredictedValueFor x
lagrange :: Double -> [(Double, Double)] -> (Double, Double)
lagrange x xys =
  let len = length xys  -- Length of the list of data points
      -- Numerator of the Lagrange interpolation term for a specific index
      numerator index = 
        product $ map (\i -> x - fst (xys !! i)) $ filter (/= index) [0 .. len - 1]
      -- Denominator of the Lagrange interpolation term for a specific index
      denominator index = 
        product $ map (\i -> fst (xys !! index) - fst (xys !! i)) $ filter (/= index) [0 .. len - 1]
   in -- Calculate the predicted y value for the given x using the Lagrange interpolation formula
      (x, sum [snd (xys !! i) * numerator i / denominator i | i <- [0 .. len - 1]])

-- Lagrange interpolation for a list of x values
lagrangeList :: [Double] -> [(Double, Double)] -> [(Double, Double)]
lagrangeList xs xys = [lagrange x xys | x <- xs]  -- Apply lagrange interpolation to each x in xs

-- Newton interpolation formula for a single point
-- PointToPredict x -> [(xi, yi)] -> PredictedValueFor x
newton :: Double -> [(Double, Double)] -> (Double, Double)
newton x xys = helper 0 0 1 xys  -- Start recursion with initial values
  where
    -- Helper function for Newton interpolation
    -- `ind` is the degree of the divided difference, `res` is the result so far,
    -- `prod` is the accumulated product of terms (x - xi).
    helper _ res _ [] = (x, res)  -- Base case: return the result when the list is empty
    helper ind res prod (xy' : xys') = 
      -- Recursive call, calculate the new result by adding the product of
      -- the previous term and the divided difference for the current term.
      helper (ind + 1) (res + prod * dividedDifference ind 0 xys) 
             (prod * (x - fst xy')) xys'

-- Newton interpolation for a list of x values
newtonList :: [Double] -> [(Double, Double)] -> [(Double, Double)]
newtonList xs xys = [newton x xys | x <- xs]  -- Apply Newton interpolation to each x in xs

-- Divided difference function used for Newton interpolation
-- It calculates the value of the divided difference for a given degree of interpolation.
dividedDifference :: Int -> Int -> [(Double, Double)] -> Double
dividedDifference 0 ind xys = snd $ xys !! ind  -- Base case: for degree 0, return the y-value at index `ind`
dividedDifference degree ind xys =
  -- Recursively calculate the divided difference using the formula:
  -- (f[x0, x1, ..., xk] = (f[x1, ..., xk] - f[x0, ..., xk-1]) / (xk - x0))
  (dividedDifference (degree - 1) (ind + 1) xys - dividedDifference (degree - 1) ind xys)
    / (fst (xys !! (ind + degree)) - fst (xys !! ind))  -- The denominator is the difference in x-values
