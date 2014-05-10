{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.Net where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Data.Attoparsec            (parseOnly)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.Map                   as M
import           Data.Monoid
import           MirakelBot.Handlers
import           MirakelBot.Message.Receive
import           MirakelBot.Message.Send
import           MirakelBot.Types
import           Network
import           System.IO
import           System.Time
import           Text.Printf

connect :: BotConfig -> IO BotEnv
connect config = notify $ do
        startTime <- getClockTime
        h <- connectTo (view botServer config) (view botPort config)
        muserlist <- newMVar M.empty
        hSetBuffering h NoBuffering
        return (BotEnv config h startTime muserlist)
    where
        notify = bracket_
            (printf "Connecting to %s ... " (view botServer config) >> hFlush stdout)
            (putStrLn "done.")

run :: [Irc ()] -> Irc ()
run handlers = do
    sequence_ handlers
    cfg <- view botConfig
    let nick = view botNick cfg
    let chan = view botChan cfg
    let real = view botRealName cfg
    h <- asks _socket
    liftIO $ send' h $ ServerMessage Nothing (Command "NICK") (Param <$> [nick])
    liftIO $ send' h $ ServerMessage Nothing (Command "USER") (Param <$> [nick, "0", "*", ":" <> real])
    liftIO $ send' h $ ServerMessage Nothing (Command "JOIN") (Param <$> [chan])
    asks _socket >>= listen

listen :: Handle -> Irc ()
listen h = forever $ do
        rawMessage <- B.init `fmap` liftIO (B.hGetLine h)
        liftIO $ putStrLn $ BC.unpack rawMessage
        let msg = parseOnly parseMessage rawMessage
        either handleError handleSucc msg
    where
        printError = liftIO . print

        handleError :: String -> Irc ()
        handleError err= do
            lastMessage .= Nothing
            printError err

        handleSucc :: Message -> Irc ()
        handleSucc msg = do
            lastMessage .= Just msg
            handleMessage msg
