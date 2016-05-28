-- since Haskell IO is lazy, this forces IO to happen in the sequence we want
import System.IO (hFlush, stdout)
flush :: IO ()
flush = hFlush stdout

-- guess what this does :^)
-- TODO: add an approximation/'close enough' factor
isInt :: RealFrac a => a -> Bool
isInt num = (floor num) == (ceiling num)

-- the main logic of our calculator
-- `rpnFolder` is a folding function that takes three arguments
-- 1) the previous answer that has been calculated
-- 2) the current calculation stack
--      since we are using a Reverse Polish method, the right operand is
--      'higher' on the stack than the left operand
--      (rNum is at the top of the stack)
-- 3) the current token (i.e. word) on the stack
-- the return value is the 'next' calculation stack, with the current
-- calculation result popped on
rpnFolder :: (RealFrac a, Floating a, Read a) => a -> [a] -> String -> [a]
rpnFolder _ (rNum:lNum:rest) "+"    = (lNum + rNum):rest
rpnFolder _ (rNum:lNum:rest) "-"    = (lNum - rNum):rest
rpnFolder _ (rNum:lNum:rest) "*"    = (lNum * rNum):rest
rpnFolder _ (rNum:lNum:rest) "/"    = (lNum / rNum):rest
rpnFolder _ (rNum:lNum:rest) "**"   = (lNum ** rNum):rest
rpnFolder _ (rNum:lNum:rest) "<<"   = (lNum * (2 ** rNum)):rest
rpnFolder _ (rNum:lNum:rest) ">>"   = (lNum / (2 ** rNum)):rest
rpnFolder prevAns stack      "ans"  = prevAns:stack
rpnFolder _       stack      "pi"   = pi:stack
rpnFolder _       stack      "e"    = (exp 1):stack
rpnFolder _ stack numberString      = (read numberString):stack

-- run `rpnFolder` over `parse` (word tokens) to get result calculation stack
-- notice we curry `rpnFolder` w/ `prevAns` to get the correct type for foldl
rpnSolver :: (RealFrac a, Floating a, Read a) => [String] -> a -> [a]
rpnSolver parse prevAns = foldl (rpnFolder prevAns) [] parse

-- using `parse` (list of word tokens) and the previous calculations' result,
-- get the new calculation stack and pop off the result
solve :: (RealFrac a, Floating a, Read a) => [String] -> a -> a
solve parse prevAns =
  let resultStack = rpnSolver parse prevAns
  in head resultStack

-- the main repl loop
-- notice how all of our IO is done here
-- this allows us to have all of our other functions absolutely pure!
loop :: (RealFrac a, Floating a, Read a, Show a) => a -> IO ()
loop ans = do
  -- prompt for input
  putStr "> "
  flush
  -- get the input
  input <- getLine
  -- tokenize the input into an array of strings
  let parse = words input
  -- get the solution to the input calculation
  let solution = solve parse ans
  -- if the solution is an int, stringify it like one
  let solutionFormat = if isInt solution then show (floor solution) else show solution
  -- get the output string and print it
  let solutionStr = "# " ++ solutionFormat
  putStrLn solutionStr
  -- get the next input!
  loop solution

main :: IO ()
main = do
  putStrLn "Reverse Polish Notation Calculator by LOZORD 2016"
  -- start the repl with "ans" = 0
  loop 0
