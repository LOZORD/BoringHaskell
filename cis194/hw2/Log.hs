-- CIS 194 Homework 2

module Log where

import Control.Applicative

-- we create a data type for log messages
data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)
-- MessageTypes can be Info, Warning, or Error
-- When they are Errors, they have an Int value associated with them
-- the `deriving` part allows us to print MessageTypes and use `==` on them

-- basic typedef
type TimeStamp = Int

-- we create another data type to encapsulate each log message
data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)
-- if the log message is NOT formated properly, we use the Unknown String version
-- the `deriving` stuff here does the same as above

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file
