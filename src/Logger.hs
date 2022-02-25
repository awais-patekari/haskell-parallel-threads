{-|
Module      : Logger
Description : Handles output
License     : GPL-3
Maintainer  : awaispatekari119@gmail.com
Stability   : experimental
Portability : POSIX

This module handles the output of the app.
It gives the user options to choose between different output choices.
-}
module Logger where

import Types
import Data.Maybe
import Control.Concurrent
import Data.List

-- |Displays total messages sent in the social network.
dispalyResult :: [Message] -> IO ()
dispalyResult resultData = do
    putStrLn "----------------------------------------------------------------------------------------------------------------"
    putStrLn listHead
    putStrLn "----------------------------------------------------------------------------------------------------------------"
    mapM_ (putStrLn . resultToString) resultData
    putStrLn "----------------------------------------------------------------------------------------------------------------"

-- |Displays headers for the table for all the messages exchanged. Used inside dispalyResult.
listHead :: String
listHead = " || "
    ++ "Sender" ++ "           || "
    ++ "Receiver" ++ "          || "
    ++ "Message" ++ "  || "

-- |Parses a Message type to a string. Used inside dispalyResult.
resultToString :: Message -> String
resultToString result = " | "
    ++ (userName $ sender result) ++ "     "
    ++ (userName $ receiver result) ++ "     "
    ++ (message result) ++ " | "

-- |Displays total messages sent or received by a user in the social network.
displayCount :: [([String],Int)] -> String -> IO ()
displayCount resultData logType = do
    putStrLn "----------------------------------------------------------------------------------------------------------------"
    putStrLn $ listHeadCount logType
    putStrLn "----------------------------------------------------------------------------------------------------------------"
    mapM_ (putStrLn . resultToStringCount) resultData
    putStrLn "----------------------------------------------------------------------------------------------------------------"

-- |Displays headers for the messages sent/recieved count table. Used inside displayCount.
listHeadCount :: String -> String
listHeadCount logType = do
     case () of
      ()| logType == "receivedCount" -> do
            " || "
            ++ "User" ++ "          || "
            ++ "Messages Received" ++ "   || "
        | logType == "sentCount"-> do
            " || "
            ++ "User" ++ "          || "
            ++ "Messages Sent" ++ "   || "

-- |Parses a touple to a string. Used inside displayCount
resultToStringCount :: ([String],Int) -> String
resultToStringCount result = " || "
            ++ (fst result !! 0) ++ "     "
            ++ (show $ snd result) ++ "  "

-- |Logs output for Count of messages each user Received
logReceivedOutput :: [Message] -> IO ()
logReceivedOutput messagesSent = do
    let receivers = map (\x -> userName $ receiver x) messagesSent
    let freq = map (\x -> ([head x], length x)) . group . sort $ receivers
    displayCount freq "receivedCount"

-- |Logs output for Count of messages each user Sent
logSentOutput :: [Message] -> IO ()
logSentOutput messagesSent = do
    let senders = map (\x -> userName $ sender x) messagesSent
    let freq = map (\x -> ([head x], length x)) . group . sort $ senders
    displayCount freq "sentCount"

-- |Handles user input
outputLogger :: [Message] -> MVar Bool -> IO ()
outputLogger messagesSent outputLogFlag = do
    putStrLn "---------------------------------"
    putStrLn "  Choose an Option."
    putStrLn "  (1) List of number of messages each user Received.    "
    putStrLn "  (2) List of number of messages each user Sent     "
    putStrLn "  (3) All Messages Exchanged     "
    putStrLn "  (4) Quit                       "
    putStrLn "---------------------------------"
    checkOption <- getLine
    case () of
      ()| checkOption == "1" -> do
            logReceivedOutput messagesSent
            outputLogger messagesSent outputLogFlag
        | checkOption == "2"-> do
            logSentOutput messagesSent
            outputLogger messagesSent outputLogFlag
        | checkOption == "3"-> do
            dispalyResult messagesSent
            outputLogger messagesSent outputLogFlag
        | checkOption == "4"-> do
            putMVar outputLogFlag True
        |otherwise -> do
            print "Invalid option"
            outputLogger messagesSent outputLogFlag
