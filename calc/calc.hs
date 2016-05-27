import System.IO (hFlush, stdout)

flush :: IO ()
flush = hFlush stdout

isInt :: RealFrac a => a -> Bool
isInt num = (floor num) == (ceiling num)

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

rpnSolver :: (RealFrac a, Floating a, Read a) => [String] -> a -> [a]
rpnSolver parse prevAns = foldl (rpnFolder prevAns) [] parse

solve :: (RealFrac a, Floating a, Read a) => [String] -> a -> a
solve parse prevAns =
  let resultStack = rpnSolver parse prevAns
  in head resultStack

loop :: (RealFrac a, Floating a, Read a, Show a) => a -> IO ()
loop ans = do
  putStr "> "
  flush
  input <- getLine
  let parse = words input
  let solution = solve parse ans
  let solutionFormat = if isInt solution then show (floor solution) else show solution
  let solutionStr = "# " ++ solutionFormat
  putStrLn solutionStr
  loop solution

main :: IO ()
main = do
  putStrLn "Reverse Polish Notation Calculator by LOZORD 2016"
  loop 0
