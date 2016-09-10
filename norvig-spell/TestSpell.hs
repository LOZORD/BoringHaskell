module TestSpell where

import Data.List (intercalate)
import qualified Spell
import qualified Data.Map.Strict as Map

getTestResult :: (Fractional v, Ord v) => Map.Map String v -> (String, String) -> [String] -> [String]
getTestResult myWords (input, expectedOutput) logs =
  let actualResult = Spell.correction myWords input
      logUpdate = if actualResult == expectedOutput
                    then "PASS"
                    else "FAIL (" ++ input ++ ")"
  in logUpdate:logs

runTests'' :: (Fractional v, Ord v) => Map.Map String v -> [(String, String)] -> [String]
runTests'' myWords inputsAndOutputs = foldr (getTestResult myWords) [] inputsAndOutputs

runTests' :: (Fractional v, Ord v) => Map.Map String v -> [(String, String)] -> String
runTests' myWords inputsAndOutputs =
  let logs = runTests'' myWords inputsAndOutputs
  in if (all (== "PASS") logs)
        then "ALL TESTS PASS"
        else "FAILURES \n\t" ++ intercalate "\n\t" logs

runTests =
  let wordList = [  "spelling", "corrected", "bicycle", "inconvenient",
                    "arranged", "poetry", "word"  ]
      -- faking our test data ;)
      frequency = 1 / (toRational (length wordList))
      primordialMap = map (\ word -> (word, frequency)) wordList
      myWords = Map.fromList primordialMap
      inputsAndOutputs = [  ("speling", "spelling"), ("korrectud", "corrected"),
                            ("bycycle", "bicycle"), ("inconvient", "inconvenient"),
                            ("arrainged", "arranged"), ("peotry", "poetry"),
                            ("peotryy", "poetry"), ("word", "word"),
                            ("quintessential", "quintessential")  ]
  in runTests' myWords inputsAndOutputs

