{-# LANGUAGE OverloadedStrings #-}
-- | Talk with the bot
module MirakelBot.Handlers.Talk where
import           Control.Monad
import           MirakelBot.Handlers
import           MirakelBot.Message.Send
import           MirakelBot.Types

init :: Irc ()
init = void $ registerDirectMessageHandler handleDirect

handleDirect :: Handler
handleDirect = answer "Jo"
