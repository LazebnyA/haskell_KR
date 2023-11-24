{-# LANGUAGE FlexibleInstances #-}

import Data.Ratio
import Data.Char

-- Task 1
-- part A

diff :: (Floating a) => (a -> a) -> a -> a -> a
diff f dx x = (f (x + dx) - f x) / dx

-- part B

newton_iter :: (Num a, Floating a) => (a -> a) -> (a -> a) -> a -> Int -> a
newton_iter f f' x 0 = x
newton_iter f f' x k = newton_iter f f' (x - f x / f' x) (k - 1)

task1 :: IO ()
task1 = do
  -- part A tests
  putStrLn $ show $ diff (\x -> x * x) 0.000001 5
  putStrLn $ show $ diff (\x -> x ^ 6) 0.000001 2
  putStrLn $ show $ diff (\x -> (6 * x) + 10) 0.000001 10
  putStrLn $ show $ diff sin 0.000001 0
  putStrLn $ show $ diff log 0.000001 2

  putStrLn ""

  -- part B tests
  putStrLn $ show $ newton_iter sin (\x -> diff sin 0.01 x) 0.5 100
  putStrLn $ show $ newton_iter polynomialFunc (\x -> diff polynomialFunc 0.01 x) 100 100
  where
    polynomialFunc x = x ^ 3 - 328 * x ^ 2 - 1999 * x - 1670

-- Task 2

type IntSet = (Int -> Bool)

isMember :: IntSet -> Int -> Bool
isMember f x = f x

emptySet :: IntSet
emptySet x = False

allInts :: IntSet
allInts x = True

-- interval x y contains all the integers in [x;y]
interval :: Int -> Int -> IntSet
interval lBound uBound = \x -> (x >= lBound) && (x <= uBound)

euclideanAlgorithm :: Int -> Int -> Int
euclideanAlgorithm x y
  | y == 0 = x
  | otherwise = euclideanAlgorithm y (x `mod` y)

primesForK :: Int -> IntSet
primesForK k = \x -> ((euclideanAlgorithm x k) == 1)

-- Boolean Operators
setIntersection :: IntSet -> IntSet -> IntSet
setIntersection set1 set2 = \x -> set1 x && set2 x

setUnion :: IntSet -> IntSet -> IntSet
setUnion set1 set2 = \x -> set1 x || set2 x

setComplement :: IntSet -> IntSet -> IntSet
setComplement set1 set2 = \x -> set1 x && not (set2 x)

-- Set generation
addToSet :: Int -> IntSet -> IntSet
addToSet elToAdd set1 = \x -> set1 x || x == elToAdd

deleteFromSet :: Int -> IntSet -> IntSet
deleteFromSet elToDel set1 = \x -> set1 x && x /= elToDel

-- Set equality (by using lists; it's possible only for bounded lists, so we can't use (==) for unbounded sets)
instance Eq IntSet where
  set1 == set2 = all (\x -> set1 x == set2 x) [-10000, -9999 .. 10000]




-- Task 3


data Token = TokNum Float | TokOp Char | TokVar String | TokFunc String deriving (Show, Eq)

type Stack = [Token]

parse :: String -> Stack
parse expr = process [] [] expr
  where
    process outputStack operatorStack [] = outputStack ++ operatorStack
    process outputStack operatorStack (c:cs)
      | c == '.' || isDigit c = process (outputStack ++ [TokNum (read (takeWhile (\x -> x == '.' || isDigit x) (c:cs)) :: Float)]) operatorStack (dropWhile (\x -> x == '.' || isDigit x) cs)
      | c `elem` "+-*/^%" = if (checkPrecedence c operatorStack)
                            then process outputStack (handleOperator c operatorStack) cs
                            else handleOutput outputStack operatorStack (c:cs)
      | c == '('  = process outputStack (TokOp c : operatorStack) cs
      | c == ')'  = process (outputStack ++ (takeWhile (/= TokOp '(') operatorStack)) (tail $ dropWhile (/= TokOp '(') operatorStack) cs
      | isAlpha c = if isFunction funcName then process outputStack (TokFunc funcName : operatorStack) (dropWhile isAlpha cs)
                    else process (outputStack ++ [TokVar (takeWhile isAlphaNum (c:cs))]) operatorStack (dropWhile isAlphaNum cs)
      | isSpace c = process outputStack operatorStack cs
      | otherwise = error "ERROR: Unexpected character"
      where
        funcName = takeWhile isAlpha (c:cs)
        isFunction str = str `elem` ["sin", "cos", "tan"]

        checkPrecedence :: Char -> Stack -> Bool
        checkPrecedence c [] = True
        checkPrecedence c (topOp:rest)
          | precedence c <= precedenceTop topOp = False
          | otherwise                           = True

        handleOperator :: Char -> Stack -> Stack
        handleOperator op (topOp:rest) = TokOp op : topOp : rest
        handleOperator op [] = [TokOp op]

        handleOutput :: Stack -> Stack -> String -> Stack
        handleOutput outputStack (topOp: rest) (op:cs) = process (outputStack ++ [topOp]) rest (op:cs)

        precedence :: Char -> Int
        precedence op
          | op `elem` "+-" = 1
          | op `elem` "*/%" = 2
          | op == '^' = 3
          | otherwise = 0

        precedenceTop :: Token -> Int
        precedenceTop (TokOp op) = precedence op
        precedenceTop (TokFunc func) = funcPrecedence func
        precedenceTop _ = 0

        funcPrecedence :: String -> Int
        funcPrecedence func
          | func `elem` ["sin", "cos", "tan"] = 4
          | otherwise = 0


-- Test the parser on expressions with variables and functions

eval :: Stack -> [(String, Float)] -> Float
eval tokens context = eval' tokens []
  where
    eval' :: Stack -> [Float] -> Float
    eval' [] [result] = result
    eval' (TokNum num : rest) operandStack = eval' rest (num : operandStack)
    eval' (TokVar var : rest) operandStack = case lookup var context of
      Just value -> eval' rest (value : operandStack)
      Nothing -> error $ "ERROR: Variable " ++ var ++ " not found in the context"
    eval' (TokFunc func : rest) operandStack =
      case func of
        "sin" -> eval' rest $ sin operand1 : newStack
        "cos" -> eval' rest $ cos operand1 : newStack
        "tan" -> eval' rest $ tan operand1 : newStack
        "pi"  -> eval' rest $ pi : newStack
        _     -> error $ "ERROR: Unexpected function " ++ func
      where
        operand1 = head operandStack
        newStack = tail operandStack
    eval' (TokOp op : rest) operandStack =
      case op of
        '+' -> eval' rest $ (operand2 + operand1) : newStack
        '-' -> eval' rest $ (operand2 - operand1) : newStack
        '*' -> eval' rest $ (operand2 * operand1) : newStack
        '/' -> eval' rest $ (operand2 / operand1) : newStack
        '^' -> eval' rest $ (operand2 ** operand1) : newStack
        '%' -> eval' rest $ (operand2 - fromIntegral (floor (operand2 / operand1)) * operand1) : newStack
        _   -> error "ERROR: Unexpected operator"
      where
        operand1 = head operandStack
        operand2 = head $ tail operandStack
        newStack = drop 2 operandStack



task3 :: IO ()
task3 = do
  putStrLn $ show $ parse "2 * ((((35 - 12)))) * 59"
  putStrLn $ show $ parse "54.5 - 12 + 2 * 15 ^ 9 * 59"

  putStrLn ""
  let expr1 = "10 * 2 ^ (3 - 1) * 3.5"
  let expr2 = "(10 / (2 % 2)) + 1"
  let expr3 = "((2 + 2)) + (((3 ^ 2 % 2)))"

  let tokens1 = parse expr1
  let tokens2 = parse expr2
  let tokens3 = parse expr3


  putStrLn $ "Result 1: " ++ show (eval tokens1 [])
  putStrLn $ "Result 2: " ++ show (eval tokens2 [])
  putStrLn $ "Result 3: " ++ show (eval tokens3 [])

  putStrLn ""

  let exprWithVars = "a + 2 * b"
  let exprWithFuncs = "sin(1/2) + cos(1/4)"
  let context = [("a", 3.0), ("b", 4.0)]

  let tokensWithVars = parse exprWithVars
  let tokensWithFuncs = parse exprWithFuncs

  putStrLn $ "Parsed variables: " ++ show tokensWithVars
  putStrLn $ "Parsed functions: " ++ show tokensWithFuncs
  putStrLn $ "Result with variables: " ++ show (eval tokensWithVars context)
  putStrLn $ "Result with functions: " ++ show (eval tokensWithFuncs [])


  putStrLn ""


-- Task 4

data Dual = Dual Float Float

getReal :: Dual -> Float
getReal (Dual re du) = re

getDual :: Dual -> Float
getDual (Dual re du) = du

instance Show Dual where
  show (Dual re du) = show re ++ " + " ++ show du ++ "i"

instance Eq Dual where
  {-Minimal complete definition: (==) | (/=) -}
  (==) (Dual re1 du1) (Dual re2 du2) = re1 == re2 && du1 == du2
  (/=) (Dual re1 du1) (Dual re2 du2) = re1 /= re2 || du1 /= du2

instance Num Dual where
  {-Minimal complete definition: (+), (*), abs, signum, fromInteger, (negate | (-)) -}
  (+) (Dual re1 du1) (Dual re2 du2) = Dual (re1 + re2) (du1 + du2)
  (-) (Dual re1 du1) (Dual re2 du2) = Dual (re1 - re2) (du1 - du2)
  (*) (Dual re1 du1) (Dual re2 du2) = Dual (re1 * re2) (re1 * du2 + re2 * du1)
  abs (Dual re du) = Dual (abs re) (if re < 0 then (-1) * du else du)
  signum (Dual re du) = Dual (if re < 0 then (-1) else 1) 0
  fromInteger n = Dual (fromIntegral n) 0
  negate (Dual re du) = Dual (-re) (-du)

instance Fractional Dual where
  {-Minimal complete definition: fromRational, (recip | (/))-}
  fromRational re = Dual (fromIntegral (numerator re) / fromIntegral (denominator re)) 0
  recip (Dual re du) = Dual (1 / re) (-du / (re * re))

instance Floating Dual where
  {-Minimal complete definition: everything-}
  pi = Dual pi 0
  exp (Dual re du) = Dual (exp re) (du * exp re)
  log (Dual re du) = Dual (log re) (du / re)
  sqrt (Dual re du) = Dual (sqrt re) (du / (2 * sqrt re))
  (**) (Dual re du) (Dual n _) = Dual (re ** n) (du * n * re ** (n - 1))
  logBase (Dual re1 du1) (Dual re2 _) = Dual (logBase re1 re2) (du1 / (re1 * log re2))
  sin (Dual re du) = Dual (sin re) (du * cos re)
  cos (Dual re du) = Dual (cos re) (-du * sin re)
  tan (Dual re du) = Dual (tan re) (du / cos re ** 2)
  asin (Dual re du) = Dual (asin re) (du / sqrt (1 - re ** 2))
  acos (Dual re du) = Dual (acos re) (-du / sqrt (1 - re ** 2))
  atan (Dual re du) = Dual (atan re) (du / (1 + re ** 2))
  sinh (Dual re du) = Dual (sinh re) (du * cosh re)
  cosh (Dual re du) = Dual (cosh re) (du * sinh re)
  tanh (Dual re du) = Dual (tanh re) (du / (cosh re) ** 2)
  asinh (Dual re du) = Dual (asinh re) (du / sqrt (re ** 2 + 1))
  acosh (Dual re du) = Dual (acosh re) (du / sqrt (re ** 2 - 1))
  atanh (Dual re du) = Dual (atanh re) (du / (1 - re ** 2))

derivative :: (Dual -> Dual) -> Dual -> Float
derivative f x = getDual (f x)

task4 :: IO ()
task4 = do
  -- Похибка |192560.64 - 188920.36| = 3640
  putStrLn $ "Derivative of sin(2e^(x^2)) (in x0 = pi) = " ++ show (derivative (\x -> sin (2 * exp (x ** 2))) (Dual pi 1))
  -- Оскільки arcth(x) = arth(1/x) в коді використана функція atanh(1/x)
  -- Похибка |21.897834 - 21.897837| = 0.000003
  putStrLn $ "Derivative of x^3 - ln(x^2) + 14cos(x/2) + (arcth(x))^2 (in x0 = pi) = " ++ show (derivative (\x -> x ** 3 - log (x ** 2) + 14 * cos (x / 2) + (atanh (1/x)) ** 2) (Dual pi 1))



main :: IO ()
main = do
  task1
  putStrLn ""
  task3
  task4

