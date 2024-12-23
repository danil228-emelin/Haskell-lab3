module Interpolation (lagrange, lagrangeList, newton, newtonList) where

-- PointToPredict x -> [(xi, yi)] -> PredictedValueFor x
lagrange :: Double -> [(Double, Double)] -> (Double, Double)
lagrange x xys =
  let len = length xys
      numerator index = product $ map (\i -> x - fst (xys !! i)) $ filter (/= index) [0 .. len - 1]
      denominator index = product $ map (\i -> fst (xys !! index) - fst (xys !! i)) $ filter (/= index) [0 .. len - 1]
   in (x, sum [snd (xys !! i) * numerator i / denominator i | i <- [0 .. len - 1]])

lagrangeList :: [Double] -> [(Double, Double)] -> [(Double, Double)]
lagrangeList xs xys = [lagrange x xys | x <- xs]

newton :: Double -> [(Double, Double)] -> (Double, Double)
newton x xys = helper 0 0 1 xys
  where
    helper _ res _ [] = (x, res)
    helper ind res prod (xy' : xys') = helper (ind + 1) (res + prod * dividedDifference ind 0 xys) (prod * (x - fst xy')) xys'

newtonList :: [Double] -> [(Double, Double)] -> [(Double, Double)]
newtonList xs xys = [newton x xys | x <- xs]

dividedDifference :: Int -> Int -> [(Double, Double)] -> Double
dividedDifference 0 ind xys = snd $ xys !! ind
dividedDifference degree ind xys =
  (dividedDifference (degree - 1) (ind + 1) xys - dividedDifference (degree - 1) ind xys)
    / (fst (xys !! (ind + degree)) - fst (xys !! ind))