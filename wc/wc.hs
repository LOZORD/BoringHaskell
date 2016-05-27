import System.Environment (getArgs)
import Data.List (intercalate)

-- myWC gets three ints of info
-- numLines -> number of lines
-- numWords -> number of space-delimited "chunks" (i.e. words)
-- numChars -> total number of characters
myWC :: String -> (Int, Int, Int)
myWC content =
  let numLines = length $ lines content
      numWords = length $ words content
      numChars = length content
  in  (numLines, numWords, numChars)

main :: IO ()
main = do
  files <- getArgs
  -- right now, this program only supports reading from one file
  if (length files /= 1)
    then error "Only supports `mywc <single file name>`"
    else do
      -- get the one and only file name
      let fileName = (head files)
      fileContent <- readFile fileName
      let (myLines, myWords, myChars) = myWC fileContent
      -- put tab characters between the stringified count data
      -- `intercalate` is like `join("\t")` in other languages
      let dataStr = intercalate "\t" (map (show) [myLines, myWords, myChars])
      let myPrintStr  = "\t" ++ dataStr ++ "\t" ++ fileName
      putStrLn myPrintStr
