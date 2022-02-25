{-|
Module      : Types
Description : Defines data types
License     : GPL-3
Maintainer  : awaispatekari119@gmail.com
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE DeriveGeneric #-}

module Types
    ( User (..),
    Message (..),
    userNames,
    messages
    ) where

-- |User data type
data User = User {
    userName :: String
} deriving (Show)

-- |Message data type
data Message = Message {
    sender :: User,
    receiver :: User,
    message :: String
} deriving (Show)

-- |Different predefined User names
userNames = [ 
    "Lando Norris", 
    "Lewis Hamilton", 
    "Max Verstappen", 
    "Daniel Riccardo", 
    "George Russel", 
    "Carlos Sainz", 
    "Charles Leclerc", 
    "Sergio Perez", 
    "Pierre Gasly", 
    "Sebastian Vettel"
    ]
    
-- |Different predefined messages
messages = [
    "I'm going to make him an offer he can't refuse.",
    "Toto, I've got a feeling we're not in Kansas anymore.",
    "Here's looking at you, kid.",
    "May the Force be with you.",
    "Frankly, my dear, I don't give a damn",
    "You talking to me?",
    "I love the smell of napalm in the morning.",
    "Bond. James Bond.",
    "You can't handle the truth!",
    "Say 'hello' to my little friend!"
    ]