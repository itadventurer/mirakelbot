{-# LANGUAGE OverloadedStrings #-}
-- | Talk with the bot
module MirakelBot.Handlers.Mirakel where
import           MirakelBot.Handlers
import           MirakelBot.Message.Send
import           MirakelBot.Types
import Data.Time.Clock
import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.Text as T
init :: Irc ()
init = do
    ntime <- liftIO getCurrentTime
    lastMentioning <- liftIO $ newMVar ntime
    _ <- registerMentioningHandler "Mirakel" $ handleMentioning lastMentioning
    _ <- registerMentioningHandler "mirakel" $ handleMentioning lastMentioning
    _ <- registerBangHandler "lastm" $ handleLastM lastMentioning
    return ()

handleMentioning :: MVar UTCTime -> Handler
handleMentioning _ = answer "yeah i am mirakel"

handleLastM :: MVar UTCTime -> T.Text -> Handler
handleLastM var _ = do
    ntime <- liftIO $ readMVar var
    answer (T.pack $ show ntime)
