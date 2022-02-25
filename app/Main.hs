{-|
Module      : Main
Description : Main module
License     : GPL-3
Maintainer  : awaispatekari119@gmail.com
Stability   : experimental
Portability : POSIX

Main module handles concurrency and creates different user threads. It also calls the logger module.
It creates the shares variables (MVars).
-}
module Main where

import Control.Concurrent
import System.Random
import System.IO
import System.Exit
import Logger
import UserThread

-- |This is the main function.
main = do
    putStrLn "---------------------------------"
    putStrLn $ "Initiating"
    putStrLn "---------------------------------"

    messageShared <- newMVar []
    completedFlag <- newEmptyMVar
    outputLogFlag <- newEmptyMVar
    sharedGenerator <- newMVar (mkStdGen 932497234)

    forkIO (userThread "Lewis Hamilton" messageShared completedFlag sharedGenerator)
    forkIO (userThread "Max Verstappen" messageShared completedFlag sharedGenerator)
    forkIO (userThread "Daniel Riccardo" messageShared completedFlag sharedGenerator)
    forkIO (userThread "Lando Norris" messageShared completedFlag sharedGenerator)
    forkIO (userThread "George Russel" messageShared completedFlag sharedGenerator)
    forkIO (userThread "Carlos Sainz" messageShared completedFlag sharedGenerator)
    forkIO (userThread "Charles Leclerc" messageShared completedFlag sharedGenerator)
    forkIO (userThread "Sergio Perez" messageShared completedFlag sharedGenerator)
    forkIO (userThread "Pierre Gasly" messageShared completedFlag sharedGenerator)
    forkIO (userThread "Sebastian Vettel" messageShared completedFlag sharedGenerator)
    
    done <- takeMVar completedFlag
    putStrLn "---------------------------------"
    putStrLn $ "100 messages exchanged! Stopping now."

    messagesSent <- takeMVar messageShared

    outputLogger messagesSent outputLogFlag
    logged <- takeMVar outputLogFlag

    die("Hope you've enjoyed using the app!")
