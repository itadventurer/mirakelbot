{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.Handlers.Fun where

import           Control.Monad
import qualified Data.Text               as T
import           MirakelBot.HandlerHelpers
import           MirakelBot.Message.Send
import           MirakelBot.Types
import           MirakelBot.Handlers
import Control.Lens
import qualified Data.Map as M

init :: Irc ()
init = void . registerBangHandlerWithHelp "parrot" "Parrot a user" $ \user -> do
    c <- getCurrentChannel
    ul <- getUserList c
    let nick = Nick user
    guard $ nick `M.member` ul
    
    i <- runIrc . registerHandler $ do
        msg <- getMessage
        guard $ msg ^? privateSender._Just == Just nick
        answer $ msg ^. privateMessage

    _ <- runIrc . registerBangHandler (T.pack "unparrot") $ \user' -> do
        guard (user' == user)
        runIrc $ unregisterHandler i
        unregisterSelf

    return ()

