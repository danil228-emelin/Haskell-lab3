import Interpolation (lagrange, lagrangeList, newton, newtonList)
import Test.HUnit (Test (..), assertBool, assertEqual, runTestTTAndExit)

main :: IO ()
main = do
  runTestTTAndExit tests

tests :: Test
tests =
  TestList
    [ TestLabel "test newton" testNewton,
      TestLabel "test newtonList" testNewtonList,
      TestLabel "test lagrange" testLagrange,
      TestLabel "test lagrangeList" testLagrangeList
    ]

withinRange :: (Ord a, Num a) => a -> a -> a -> Bool
withinRange eps expected actual = expected - eps <= actual && actual <= expected + eps

testNewton :: Test
testNewton =
  TestCase
    ( do
        -- basic y = 3x
        let x_3 = [(3, 9), (10, 30), (100, 300)]
        assertEqual "Test y = 3x newton interpolation" (5, 15) (newton 5 x_3)
        -- basic y = -2x
        let minus_2x = [(3, -6), (10, -20), (100, -200)]
        assertEqual "Test y = -2x newton interpolation" (25, -50) (newton 25 minus_2x)
    )

testNewtonList :: Test
testNewtonList =
  TestCase
    ( do
        let x_5 = [(x, 5 * x) | x <- [1 .. 10]]
        let vals = [20 .. 30]
        assertEqual "Test newton interpolation for a list of values" (map (\x -> (x, 5 * x)) vals) (newtonList vals x_5)
    )

testLagrange :: Test
testLagrange =
  TestCase
    ( do
        -- basic y = 3x
        let x_3 = [(3, 9), (10, 30), (100, 300)]
        let withinEps = withinRange 0.01
        assertBool "Test y = 3x lagrange interpolation" (snd (lagrange 5 x_3) `withinEps` 15)
        let minus_2x = [(3, -6), (10, -20), (100, -200)]
        assertBool "Test y = -2x lagrange interpolation" (snd (lagrange 25 minus_2x) `withinEps` (-50))
    )

testLagrangeList :: Test
testLagrangeList =
  TestCase
    ( do
        let x_3 = [(x, 3 * x) | x <- [1 .. 10]]
        let vals = [40 .. 50]
        assertEqual "Test lagrange interpolation for a list of values" (map (\x -> (x, 3 * x)) vals) (lagrangeList vals x_3)
    )