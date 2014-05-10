module MirakelBot.Handlers.Users where

import           Control.Monad
import           Data.Text               as T
import           MirakelBot.HandlerHelpers
import           MirakelBot.Message.Send
import           MirakelBot.Types
import           MirakelBot.Handlers
import Control.Lens

init :: Irc ()
init = void $ registerBangHandler (T.pack "users") $ \_ -> do
    msg <- getMessage
    let dest = msg ^? privateDestination
    case dest of
        Just cs -> forM_ cs $ \c ->
            case c of
                ToChannel c' -> do
                    users <- getUserList c'
                    answer (T.pack $ show users)
                _ -> return ()
        _ -> return ()
