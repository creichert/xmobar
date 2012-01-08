{-# OPTIONS -Wall -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.IrssiNotify
-- Copyright   :  (c) Christopher Reichert
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Christopher Reichert <creichert07@gmail.com>
-- Stability   :  expiremental
-- Portability :  unportable
--
-- Displays irssi pings to your xmobar. Intended to read pings written
-- to a fifo pipe.
--
-- Ex:
--  Using fnotify.pl with irssi, have all pings written to a fifo pipe in
--  your .irssi directory.
--
--  Then add this command to your Config {} in .xmobarrc.
--      Run IrssiNotify "/path/to/fifo_pipe"
--
--  Now you have access to the alias `IrssiNotify' to write these to your
--  template.
--
--  TODO:
--      - Hide ping when irssi is focused.
--      - User specified truncation in .xmobarrc template?
-----------------------------------------------------------------------------

module Plugins.IrssiNotify where

import Plugins
import System.IO


data IrssiNotify = IrssiNotify FilePath
    deriving (Read, Show)


-- cb :: String -> IO ()
instance Exec IrssiNotify where
    alias (IrssiNotify _) = "IrssiNotify"
    start (IrssiNotify fp) cb = do
        h <- openFile fp ReadWriteMode
        _ <- forever (getPing h >>= cb)
        putStrLn ""
      where forever x = x >> forever x

-- this code will be used as our discovery mechanism for
-- when pings have been seen.
#if 0
        forever (getPing h >>= cb) (irssiFocused fp >>= cb)
      where forever x y = x >> y >> forever x y

-- if irssi has been focused then we can dissapear the pings
irssiFocused :: FilePath -> IO String
irssiFocused _ = return ""
#endif


getPing :: Handle -> IO String
getPing h = do
    ping <- hGetLineSafe h
    let msg = format ping
    return msg


format :: String ->  String
format msg = (formatNick msg) ++ " " ++ (formatMsg msg)


formatNick :: String -> String
formatNick msg = nickColor $ head $ words truncatedMsg
    where truncatedMsg = take 14 msg


formatMsg :: String -> String
formatMsg msg = msgColor $ unwords $ tail $ words truncatedMsg
    where truncatedMsg = take 40 msg


nickColor :: String -> String
nickColor n = "<fc=red>" ++ n ++ "</fc>"


msgColor :: String -> String
msgColor n = "<fc=green>" ++ n ++ "</fc>"
