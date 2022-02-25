{-|
Module      : UserThread
Description : Creates different user threads
License     : GPL-3
Maintainer  : awaispatekari119@gmail.com
Stability   : experimental
Portability : POSIX

This module creates threads for users. Each thread randomly sends a message to a random user.
The message is selected from a set of predefined messages in Types module.
Once 100 messages are shared in total, it stops sending messages.
-}
module UserThread where

import Types
import Control.Concurrent
import System.Random
import Data.List

-- |Picks a random username apart from it's own from a given list of users. It returns the userName and a new generator for randomR.
pickRandomUserName :: String -> StdGen -> (String, StdGen)
pickRandomUserName user gen = do
    let newL = delete user userNames
    let rdn = randomR (0, length newL - 1) gen
    let username = newL !! fst (rdn)
    (username, snd(rdn))

-- |Picks a random message from a given list of messages. It returns the message and a new generator for randomR.
getMessage :: StdGen -> (String, StdGen)
getMessage gen = do
    let rdn = randomR (0, length messages - 1) gen
    let msg = messages !! fst (rdn)
    (msg, snd(rdn))

-- |Handles each user thread. Sends a random message to a random user after a random interval.
userThread :: String -> MVar [Message] -> MVar Bool -> MVar StdGen -> IO ()
userThread user messageShared completedFlag sharedGenerator = do
    messageList <- takeMVar messageShared
    if length messageList < 100 then do
        rdnGen <- takeMVar sharedGenerator
        let (receiverName, gen) = pickRandomUserName user rdnGen

        let (rdnMessage, gen1) = getMessage gen
        putMVar sharedGenerator gen1

        let sender = User {
            userName = user
        }
        let receiver = User {
            userName = receiverName
        }
        let message = Message {
            sender = sender,
            receiver = receiver,
            message = rdnMessage
        }
        
        let newMessageList = message : messageList
        putMVar messageShared newMessageList
        putStrLn $ user ++ " sent a message to " ++ userName receiver

        rdnG <- readMVar sharedGenerator
        threadDelay (fst (randomR (1000, 10000) (rdnG)))

        userThread user messageShared completedFlag sharedGenerator
    else do
        putMVar messageShared messageList
        putMVar completedFlag True
