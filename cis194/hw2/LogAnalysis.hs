{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

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

-- Exercise 2 TODO
insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined
--insert (Unknown _) tree = tree
--insert logMess tree = insert' logMess tree tree

--insert' :: LogMessage -> MessageTree -> MessageTree -> MessageTree
--insert' logMess 
