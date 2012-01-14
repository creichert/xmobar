{-# OPTIONS -Wall -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.IrcNotifier
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
--      Run IrcNotifier "/path/to/fifo_pipe"
--
--  Now you have access to the alias `IrcNotifier' to write these to your
--  template.
--
--  ISSUES:
--      - I can't seem to find a way to tell if the user has focused
--        the "irssi" client specifically. It's not an X Window application
--        and doesn't inheret it's parent windows pid when run inside
--        screen.
-----------------------------------------------------------------------------

module Plugins.IrcNotifier where

import Plugins
import System.IO
import System.Process
import Control.Concurrent

import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types

type IrcClient = String

data IrcNotifier = IrcNotifier FilePath IrcClient
    deriving (Read, Show)


-- cb :: String -> IO ()
instance Exec IrcNotifier where
    alias (IrcNotifier _ _) = "IrcNotifier"
    start (IrcNotifier fp clt) cb = do
        h <- openFile fp ReadWriteMode

        {- Fork off the event listener for 
           window changing events -}
        _ <- forkIO (forever (ircFocused clt cb))

        {- List for pings forever. -}
        _ <- forever (getPing h >>= cb)
        return ()
      where forever x = x >> forever x


-- if irssi has been focused then we can dissapear the pings
ircFocused :: IrcClient -> (String -> IO ()) -> IO ()
ircFocused clt cb = do
    threadDelay 1000000
    dpy <- openDisplay ""
    (w, _) <- getInputFocus dpy
    pid <- currentWindowPid w
    ircpid <- getIrcPid clt
    if pid == ircpid
        then return "" >>= cb
        else return ()


getIrcPid :: IrcClient -> IO String
getIrcPid clt =
    if clt == "irssi"
        then getppid clt
        else (getpid clt)


getpid :: String -> IO String
getpid clt = do
    pidstr <- readProcess "/bin/sh" ["-c", "ps x | grep " ++ clt ++ " | grep -v grep"] []
    let x = head $ words pidstr
    return x


getppid :: IrcClient -> IO String
getppid clt = undefined


{-
  Ideally, we would retrieve the pid by looking
  at WM properties. However, the library doesn't
  seem to work with non-standardized properties.

    atom <- internAtom dpy "_NET_WM_PID" False
    xprop <- getTextProperty dpy w atom

    wpl <- wcTextPropertyToTextList dpy xprop
-}
currentWindowPid :: XID -> IO String
currentWindowPid xid = do
    pidS <- readProcess "/bin/sh" ["-c", "xprop -id " ++ (show xid) ++ " _NET_WM_PID"] []
    let pid = last $ words pidS
    return pid


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
msgColor n = "<fc=purple>" ++ n ++ "</fc>"

