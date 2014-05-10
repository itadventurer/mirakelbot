module MirakelBot.Handlers.Users where

import Data.Foldable

import           Control.Monad hiding (forM_)
import qualified Data.Text               as T
import           MirakelBot.HandlerHelpers
import           MirakelBot.Message.Send
import           MirakelBot.Types
import           MirakelBot.Handlers
import Control.Lens
import Data.Maybe

init :: Irc ()
init = void $ registerBangHandler (T.pack "users") $ \_ -> do
    msg <- getMessage

    let toChannel x = case x of ToChannel c -> Just c; _ -> Nothing

    let mdest = msg ^? privateDestination
    forM_ mdest $ \dest ->
        forM_ (catMaybes $ map toChannel dest) $ \c -> do
            users <- getUserList c
            answer (T.pack $ show users)
