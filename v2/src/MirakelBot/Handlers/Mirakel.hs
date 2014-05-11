{-# LANGUAGE OverloadedStrings #-}
-- | Talk with the bot
module MirakelBot.Handlers.Mirakel where

import           MirakelBot.HandlerHelpers
import           MirakelBot.Handlers
import           MirakelBot.Message.Send
import           MirakelBot.Types
import Data.Time.Clock
import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Monoid

masters :: [Nick]
masters = [Nick "azapps",Nick "weiznich"]

init :: Irc ()
init = do
    ctime <- liftIO getCurrentTime
    let ntime = addUTCTime (fromIntegral (-5*600 :: Int)) ctime
    lastMentioning <- liftIO $ newMVar ntime
    _ <- registerMentioningHandler "Mirakel" $ handleMentioning lastMentioning
    _ <- registerMentioningHandler "mirakel" $ handleMentioning lastMentioning
    _ <- registerBangHandler "lastm" $ handleLastM lastMentioning
    return ()

handleMentioning :: MVar UTCTime -> Handler ()
handleMentioning var = do
    lastMentioning <- liftIO $ readMVar var
    currentTime <- liftIO getCurrentTime
    liftIO $ modifyMVar_ var $ \_ -> return currentTime
    channel <- getCurrentChannel
    onlineUsers <- filterM (userIsOnline channel) masters
    when (diffUTCTime currentTime lastMentioning > fromIntegral (10 :: Int)) $
        if null onlineUsers then
            answer "I am the MirakelBot. My developers are currently offline. If your problem persist please write us an email to mirakel@azapps.de or join the #mirakel channel"
        else
            answer $ "I am the MirakelBot. If you have questions just ask " <> T.intercalate " or " (map getNick onlineUsers) <>  " or join the #mirakel channel"

handleLastM :: MVar UTCTime -> T.Text -> Handler ()
handleLastM var _ = do
    ntime <- liftIO $ readMVar var
    answer (T.pack $ show ntime)
