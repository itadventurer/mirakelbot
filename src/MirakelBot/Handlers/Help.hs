{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.Handlers.Help where

import           Control.Monad             hiding (forM_)
import           Data.Foldable
import qualified Data.Map                  as M
import           Data.Monoid
import qualified Data.Text                 as T
import           MirakelBot.HandlerHelpers
import           MirakelBot.Message.Send
import           MirakelBot.Types

init :: Irc ()
init = void $ registerBangHandlerWithHelp "help" "show this help message" $ \_ -> do
    helps <- getHelp
    hotword <- getHotword
    forM_ (M.toList helps) $ \(c,h) -> answer $ T.unwords [hotword <> c <> ":", h]
