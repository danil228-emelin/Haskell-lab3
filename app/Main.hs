module Main (main) where

import Data.List.Split (splitOn)
import Interpolation (lagrangeList, newtonList)
import Options.Applicative
import System.IO
import Text.Read (readMaybe)
import Control.Monad (forM_, forM)
import Prelude

---- options and parsing ----

data Options = Options
  { step :: Double,
    window :: Int,
    method :: [String]
  }

stepOption :: Parser Double
stepOption =
  option
    auto
    ( long "step"
        <> help "Step value"
        <> metavar "DOUBLE"
    )

windowOption :: Parser Int
windowOption =
  option
    auto
    ( long "window"
        <> help "window size"
        <> metavar "INTEGER"
    )

methodOption :: Parser [String]
methodOption =
  some $
    strOption
      ( long "method"
          <> help "Method name (e.g. 'lagrange')"
          <> metavar "[STRING]"
      )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> stepOption
    <*> windowOption
    <*> methodOption

fromMaybe :: Maybe a -> a
fromMaybe Nothing = error "nothing to unpack"
fromMaybe (Just x) = x

toPair :: String -> String -> (Double, Double)
toPair sep fromStr = (head l, l !! 1)
  where
    l = map (fromMaybe . stringToDouble) $ splitOn sep fromStr

stringToDouble :: String -> Maybe Double
stringToDouble = readMaybe

maybeReadPoint :: IO (Maybe (Double, Double))
maybeReadPoint = do
  eof <- isEOF
  if eof
    then return Nothing
    else do
      Just . toPair ";" <$> getLine

---- main routine ----

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Linear interpolation."
            <> header "Linear interpolation"
        )

readPoints :: IO [(Double, Double)]
readPoints = do
  maybePoint <- maybeReadPoint
  case maybePoint of
    Just pair -> readPoints >>= (\x -> return (pair : x))
    Nothing -> return []

validMethods :: [String]
validMethods = ["lagrange", "newton"]

validateMethods :: [String] -> Bool
validateMethods = foldr (\m -> (&&) (m `elem` validMethods)) True

interpolatorFromName :: String -> ([Double] -> [(Double, Double)] -> [(Double, Double)])
interpolatorFromName name = case name of
  "newton" -> newtonList
  "lagrange" -> lagrangeList
  _ -> error "No such interpolator"

runWithOptions :: Options -> IO ()
runWithOptions (Options mStep mWindow mMethod) = do
  putStrLn $ "Step: " ++ show mStep
  putStrLn $ "Window: " ++ show mWindow
  putStrLn $ "Method: " ++ show mMethod
  if not $ validateMethods mMethod
    then error "Bad method"
    else cliRoutine mStep mWindow mMethod

cliRoutine :: Double -> Int -> [String] -> IO ()
cliRoutine step' window' methods = do
  points <- readPoints
  let pts = take window' points
  mapM_ (interpolateInInterval_ pts (fst $ head pts) (fst $ pts !! (window' - 1)) step') methods

interpolateInInterval_ :: [(Double, Double)] -> Double -> Double -> Double -> String -> IO ()
interpolateInInterval_ vals fromX toX freq ipol = do
  let pts = [fromX, (fromX + freq) .. toX]
  let a = interpolatorFromName ipol pts vals
  print a