{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.Handlers.ServerComm where

import           Data.Foldable
import           MirakelBot.Handlers
import           MirakelBot.Message.Send
import           MirakelBot.Types
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import Data.Maybe
import Control.Monad
import Data.List

commHandler :: [Handler ()]
commHandler = [handlePing,handleNameReply,handleUserEvent]

init :: Irc ()
init = do
    traverse_ registerHandler commHandler
    return ()

handlePing :: Handler ()
handlePing =do 
            msg <- getMessage
            case msg of
                ServerMessage {_serverCommand = PING, _serverParams = p} -> 
                    send msg {
                          _serverPrefix = Nothing
                        , _serverCommand = PONG
                        , _serverParams = p
                        }
                _ -> return ()

handleNameReply :: Handler ()
handleNameReply = do
    msg <- getMessage
    case msg of
        ServerMessage {_serverCommand = NumericCommand 353, _serverParams = p} -> do
            let [Param channelname, Param usernames] = [last $ Prelude.init p,last p]
            let channel = Channel channelname
            let users = M.fromList $ map processUser $ T.words usernames
            modifyUserList channel $ M.union users
            where
                processUser :: Text -> (Nick, UserMode)
                processUser user = case T.uncons user of
                    Just ('@',u) -> (Nick u, ModeOperator)
                    Just ('+',u) -> (Nick u, ModeVoice)
                    Just _ -> (Nick user, ModeNormal)
                    Nothing -> error "Empty text in processUser"
        _ -> return ()

handleUserEvent :: Handler ()
handleUserEvent = do
    msg <- getMessage
    case msg of
        ServerMessage {_serverCommand = cmd
                      , _serverParams = p
                      , _serverPrefix = Just (NickPrefix {prefixNick=nick})} -> do
            let channels = mapMaybe toChannel p
            for_ channels $ \channel ->
                case cmd of
                    JOIN -> addUser channel nick ModeNormal
                    PART -> delUser channel nick
                    MODE -> do
                        let newMode = getMode p
                        unless (isNothing newMode) $ uncurry (addUser channel) $ fromJust newMode
                        return ()
                    _ -> return ()
            where
                getMode :: [Param] -> Maybe (Nick,UserMode)
                getMode params = 
                    let (modes,users) = partition ((||) <$> ("+" `T.isPrefixOf`) <*> ("-" `T.isPrefixOf`)) $ map getParam $ tail params
                        user = Nick $ head users
                    in case join $ T.uncons <$> headMaybe modes of
                        Just ('-',op) -> setMode user ModeNormal op
                        Just ('+',op) -> setMode user ModeOperator op
                        _ -> Nothing
                setMode user mode op = if "o" `T.isInfixOf` op then Just (user,mode) else Nothing
                headMaybe [] = Nothing
                headMaybe (x:_) = Just x
        _ -> return ()


toChannel :: Param -> Maybe Channel
toChannel (Param channel) = if "#" `T.isPrefixOf` channel 
    then Just $ Channel channel 
    else Nothing
