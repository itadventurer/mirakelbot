{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.Handlers.Id where
import           Control.Monad
import           Data.Text                 as T
import           MirakelBot.HandlerHelpers
import           MirakelBot.Message.Send
import           MirakelBot.Types

init :: Irc ()
init = void $ registerBangHandlerWithHelp "id" "Returns the argument" handleId

handleId :: Text -> Handler ()
handleId = answer
