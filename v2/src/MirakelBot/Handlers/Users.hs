module MirakelBot.Handlers.Users where

import           Control.Lens
import           Control.Monad             hiding (forM_)
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                 as T
import           MirakelBot.HandlerHelpers
import           MirakelBot.Handlers
import           MirakelBot.Message.Send
import           MirakelBot.Types
import Data.Monoid
import qualified Data.Map as M

init :: Irc ()
init = void $ registerBangHandler (T.pack "users") $ \_ -> do
    msg <- getMessage

    let toChannel x = case x of ToChannel c -> Just c; _ -> Nothing

    let mdest = msg ^? privateDestination
    forM_ (maybeToList mdest >>= mapMaybe toChannel) $ \c -> do
        users <- getUserList c
        answer (T.unwords $ map showUser $ M.toList users)

showUser :: (Nick,UserMode) -> T.Text
showUser (Nick nick,mode) = showt mode <> nick
