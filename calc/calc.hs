import System.IO (hFlush, stdout)

flush :: IO ()
flush = hFlush stdout

parseInput :: String -> [String]
-- TODO
parseInput input = words input

isInt :: RealFrac a => a -> Bool
isInt num = (floor num) == (ceiling num)

solve :: RealFrac a => [String] -> a -> a
-- TODO
solve parse prevAns = prevAns

loop :: (RealFrac a, Show a) => a -> IO ()
loop ans = do
  putStr "> "
  flush
  input <- getLine
  let parse = parseInput input
  let solution = solve parse ans
  let solutionFormat = if isInt solution then show (floor solution) else show solution
  let solutionStr = "# " ++ solutionFormat
  putStrLn solutionStr
  loop solution

main :: IO ()
main = loop 0
