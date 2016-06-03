{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1
-- how to Read a LogMessage
parseMessage :: String -> LogMessage
parseMessage message =
  let messageChunks   = words message
      startOfMessage  = if (null messageChunks) then "" else head messageChunks
      myMessageType   = case startOfMessage of
                          "I" ->
                            let timeStamp = read (messageChunks !! 1) :: TimeStamp
                                restOfTheMessage = unwords $ drop 2 messageChunks
                            in LogMessage Info timeStamp restOfTheMessage
                          "W" ->
                            let timeStamp = read (messageChunks !! 1) :: TimeStamp
                                restOfTheMessage = unwords $ drop 2 messageChunks
                            in LogMessage Warning timeStamp restOfTheMessage
                          "E" ->
                            let errorCode = read (messageChunks !! 1) :: Int
                                timeStamp = read (messageChunks !! 2) :: TimeStamp
                            in LogMessage (Error errorCode) timeStamp (unwords $ drop 3 messageChunks)
                          _ -> (Unknown message)
  in myMessageType

parse :: String -> [LogMessage]
parse fileContents = map (parseMessage) (lines fileContents)

-- Exercise 2 TODO
insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined
--insert (Unknown _) tree = tree
--insert logMess tree = insert' logMess tree tree

--insert' :: LogMessage -> MessageTree -> MessageTree -> MessageTree
--insert' logMess 
