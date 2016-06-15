{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

-- foldl' is the more efficient version of foldl
import Data.List (foldl')
import Log

-- Exercise 1
-- how to Read a LogMessage
parseMessage :: String -> LogMessage
parseMessage message =
      -- first split on words
  let messageChunks   = words message
      -- get the first thing in the message (which should be the type)
      startOfMessage  = if (null messageChunks) then "" else head messageChunks
      myMessageType   = case startOfMessage of
                          "I" ->
                                -- read the second "word", which is the time stamp
                            let timeStamp = read (messageChunks !! 1) :: TimeStamp
                                -- the rest of the message (besides type and ts)
                                -- is just the log string
                                restOfTheMessage = unwords $ drop 2 messageChunks
                            in LogMessage Info timeStamp restOfTheMessage
                          "W" ->
                            let timeStamp = read (messageChunks !! 1) :: TimeStamp
                                restOfTheMessage = unwords $ drop 2 messageChunks
                            in LogMessage Warning timeStamp restOfTheMessage
                          "E" ->
                                -- the first thing after the Error type is the Error code
                            let errorCode = read (messageChunks !! 1) :: Int
                                -- then comes the time stamp
                                timeStamp = read (messageChunks !! 2) :: TimeStamp
                            in LogMessage (Error errorCode) timeStamp (unwords $ drop 3 messageChunks)
                          _ -> (Unknown message)
  -- finally, simply return the newly constructed MessageType
  in myMessageType

parse :: String -> [LogMessage]
-- for every line in the file, convert it to a LogMessage
parse fileContents = map (parseMessage) (lines fileContents)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
-- don't do anything with an Unknown LogMessage
insert (Unknown _) tree = tree
-- if the current tree is just a leaf, we can insert now!
insert myLM Leaf = Node Leaf myLM Leaf
-- otherwise, our insertion depends on where we have to recurse
insert myLM currTree =
      -- get the time stamp for the LogMessage we wish to insert
  let (LogMessage _ myTS _) = myLM
      -- get the left and right subtrees as well as the current LogMessage
      (Node left currLM right) = currTree
      -- and finally, get the time stamp for the current LogMessage
      (LogMessage _ currTS _) = currLM
      -- compare the ts for the LM we wish to insert with the current ts
  in  if myTS <= currTS
        -- if leq, recursively insert on the left subtree
        then (Node (insert myLM left) currLM right)
        -- otherwise, recursively insert on the right subtree
        else (Node left currLM (insert myLM right))

-- Exercise 3
build :: [LogMessage] -> MessageTree
-- iteratively build up new tree using LogMessages list and empty tree (Leaf)
-- `(flip insert)` could have been the first arg to foldl'
-- but the lambda is more explicit
build lms = foldl' (\ tree lm -> insert lm tree) Leaf lms

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
-- a leaf has no data, so there's no data to include in the result
inOrder Leaf = []
inOrder (Node left curr right) =
      -- first get the contents of the left subtree
  let leftList  = inOrder left
      -- then get then get the contents of the right subtree
      rightList = inOrder right
      -- finally, create a list of the elements in L-C-R order
  in leftList ++ [curr] ++ rightList

-- Exercise 5
-- first, we will create some helper functions
isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _)  = True
isError _                           = False

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error level) _ _) = level >= 50
isSevere _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ s) = s
getMessage (Unknown s)        = s

-- now, for the 'main' function
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong unsortedLMs =
      -- first, sort LogMessages using our tree sorting technique
  let sortedLMs = inOrder (build unsortedLMs)
      -- we only care about Error LogMessages ...
      errorLMs  = filter (isError) sortedLMs
      -- ... whose levels are geq 50
      severeLMs = filter (isSevere) errorLMs
      -- and finally, we only care about their message contents
      severeStrings = map (getMessage) severeLMs
  in severeStrings
