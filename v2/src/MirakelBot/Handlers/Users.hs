module MirakelBot.Handlers.Users where

import           Control.Monad
import           Data.Text               as T
import           MirakelBot.HandlerHelpers
import           MirakelBot.Message.Send
import           MirakelBot.Types

init :: Irc ()
init = void $ registerBangHandler (T.pack "users") $ \_ -> do
    users <- getUserList
    answer (show users)
