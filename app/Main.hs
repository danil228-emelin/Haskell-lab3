module Main (main) where

-- Importing required libraries
import Data.List.Split (splitOn)         -- For splitting strings
import Interpolation (lagrangeList, newtonList) -- Custom interpolation functions
import Options.Applicative               -- For command-line option parsing
import System.IO                         -- For handling input/output (e.g., EOF)
import Text.Read (readMaybe)             -- For safe parsing of strings to numbers

---- options and parsing ----

-- Data type for command-line options
data Options = Options
  { step :: Double,      -- Step size for interpolation
    window :: Int,       -- Window size (number of points to consider at a time)
    method :: [String]   -- List of interpolation methods to use
  }

-- Command-line option for step value
stepOption :: Parser Double
stepOption =
  option
    auto
    ( long "step"                  -- Option name
        <> help "Step value"       -- Help description
        <> metavar "DOUBLE"        -- Expected argument type
    )

-- Command-line option for window size
windowOption :: Parser Int
windowOption =
  option
    auto
    ( long "window"                -- Option name
        <> help "window size"      -- Help description
        <> metavar "INTEGER"       -- Expected argument type
    )

-- Command-line option for interpolation methods
methodOption :: Parser [String]
methodOption =
  some $                       -- Accepts multiple values (e.g., ["lagrange", "newton"])
    strOption
      ( long "method"           -- Option name
          <> help "Method name (e.g. 'lagrange')" -- Help description
          <> metavar "[STRING]" -- Expected argument type
      )

-- Combines all the individual parsers into a single options parser
optionsParser :: Parser Options
optionsParser =
  Options
    <$> stepOption              -- Step option
    <*> windowOption            -- Window option
    <*> methodOption            -- Method(s) option

-- Function to safely unpack a Maybe value or raise an error
fromMaybe :: Maybe a -> a
fromMaybe Nothing = error "nothing to unpack" -- Error if value is Nothing
fromMaybe (Just x) = x                        -- Return the value if Just x

-- Converts a string representation of a pair to a (Double, Double) tuple
toPair :: String -> String -> (Double, Double)
toPair sep fromStr = (head l, l !! 1) -- Extract the first two elements as a tuple
  where
    l = map (fromMaybe . stringToDouble) $ splitOn sep fromStr -- Split and convert strings

-- Safely converts a string to a Double
stringToDouble :: String -> Maybe Double
stringToDouble = readMaybe -- Returns Nothing if parsing fails

-- Reads a single point from input, if available
maybeReadPoint :: IO (Maybe (Double, Double))
maybeReadPoint = do
  eof <- isEOF                   -- Check if end of file has been reached
  if eof
    then return Nothing          -- No more input available
    else do
      Just . toPair ";" <$> getLine -- Parse a line into a (Double, Double) tuple

---- main routine ----

-- Main entry point of the program
main :: IO ()
main = execParser opts >>= runWithOptions -- Parse options and call runWithOptions
  where
    opts =
      info
        (optionsParser <**> helper) -- Attach a helper to the options parser
        ( fullDesc                   -- Full description
            <> progDesc "Linear interpolation."
            <> header "Linear interpolation"
        )

-- Recursively reads all input points as a list of (Double, Double)
readPoints :: IO [(Double, Double)]
readPoints = do
  maybePoint <- maybeReadPoint
  case maybePoint of
    Just pair -> readPoints >>= (\x -> return (pair : x)) -- Add point to the list
    Nothing -> return []                                  -- No more points to read

-- Valid interpolation methods
validMethods :: [String]
validMethods = ["lagrange", "newton"]

-- Validates if all given methods are in the list of valid methods
validateMethods :: [String] -> Bool
validateMethods = foldr (\m -> (&&) (m `elem` validMethods)) True -- Check all methods

-- Maps method names to their respective interpolation functions
interpolatorFromName :: String -> ([Double] -> [(Double, Double)] -> [(Double, Double)])
interpolatorFromName name = case name of
  "newton" -> newtonList      -- Newton interpolation
  "lagrange" -> lagrangeList  -- Lagrange interpolation
  _ -> error "No such interpolator" -- Error if method is invalid

-- Runs the main program logic with the parsed options
runWithOptions :: Options -> IO ()
runWithOptions (Options mStep mWindow mMethod) = do
  putStrLn $ "Step: " ++ show mStep         -- Print the step size
  putStrLn $ "Window: " ++ show mWindow     -- Print the window size
  putStrLn $ "Method: " ++ show mMethod     -- Print the methods
  if not $ validateMethods mMethod          -- Validate methods
    then error "Bad method"                 -- Exit on invalid method
    else cliRoutine mStep mWindow mMethod   -- Proceed to the main routine

-- Command-line routine for interpolation
cliRoutine :: Double -> Int -> [String] -> IO ()
cliRoutine step' window' methods = do
  points <- readPoints                -- Read all input points
  let pts = take window' points       -- Take the first window' points
  mapM_ (interpolateInInterval_ pts (fst $ head pts) (fst $ pts !! (window' - 1)) step') methods
  start points
  where
    start points' = do
      let pts = take window' points'  -- Take the next window of points
      if length pts < window'         -- Check if enough points are available
        then do
          mapM_ (interpolateInInterval_ pts (fst $ head pts) (fst $ last pts) step') methods
        else do
          let middleX = (fst (head pts) + fst (pts !! (window' - 1))) / 2 -- Midpoint
          mapM_ (interpolateInInterval_ pts middleX middleX 1) methods
          start (tail points')        -- Move to the next set of points

-- Interpolates in a given interval and prints the results
interpolateInInterval_ :: [(Double, Double)] -> Double -> Double -> Double -> String -> IO ()
interpolateInInterval_ vals fromX toX freq ipol = do
  let pts = [fromX, (fromX + freq) .. toX] -- Generate points in the interval
  putStrLn ("Interpolating with: " ++ ipol) -- Print the interpolation method
  mapM_ print (interpolatorFromName ipol pts vals) -- Perform interpolation and print results