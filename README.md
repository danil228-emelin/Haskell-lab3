# Functional programming. Assignment # 3, Interpolation.

## Описание задания, цели и требования

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.
В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую аппроксимации (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, link);
- настройки алгоритма аппроксимирования и выводимых данных должны задаваться через аргументы командной строки:
    - какие алгоритмы использовать (в том числе два сразу);
    - частота дискретизации результирующих данных;
    - и т.п.;
- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

## Реализация

### Интерполяция по Лагранжу
![изображение](https://github.com/user-attachments/assets/60791e1a-2f81-48c6-9f27-eb93fd954a90)
```haskell
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

```
### Интерполяция по Ньютону
![изображение](https://github.com/user-attachments/assets/211eba6e-9490-4c8d-9181-402bcae83d68)
```haskell
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

```
### Работа с вводом и выводом
 Модуль для работы с аргументами командной строки - Options.Applicative
### Создание парсеров 
```haskell

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
```
### Объеденение всех парсеров в один
```haskell
-- Combines all the individual parsers into a single options parser
optionsParser :: Parser Options
optionsParser =
  Options
    <$> stepOption              -- Step option
    <*> windowOption            -- Window option
    <*> methodOption            -- Method(s) option

```
### Main IO function
```haskell
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
```
```
PS C:\Users\danil_emelin1\haskell-lab3\Haskell-lab3> cabal run Haskell-lab3 -- --step 1 --window 3 --method lagrange
Step: 1.0
Window: 3
Method: ["lagrange"]
1;3
2;3
5;1
(2.0,4.0) 
Interpolating with: lagrange
(1.0,3.0)
(1.0,3.0)
```
