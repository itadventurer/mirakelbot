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
            let [Param channelname, Param usernames] = filter ((&&) <$> (/= "+") <*> (/= "@")) $ tail p
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
            for_ channels $ \channel -> do
                currentMode <- getUserMode channel nick
                case cmd of
                    JOIN -> addUser channel nick ModeNormal
                    PART -> delUser channel nick
                    MODE -> addUser channel nick $ getMode p currentMode
                    _ -> return ()
            where
                getMode :: [Param] -> Maybe UserMode -> UserMode
                getMode p cmode = 
                    let currentMode = fromMaybe ModeNormal cmode
                        pop = headMaybe $ filter ("+" `T.isPrefixOf`) $ map getParam p
                        nop = headMaybe $ filter ("-" `T.isPrefixOf`) $ map getParam p
                    in case nop of
                        Just op -> if "o" `T.isInfixOf` op then ModeNormal else currentMode
                        Nothing -> case pop of
                            Just op -> if "o" `T.isInfixOf` op then ModeOperator else currentMode
                            _ -> currentMode
                headMaybe [] = Nothing
                headMaybe (x:_) = Just x
        _ -> return ()


toChannel :: Param -> Maybe Channel
toChannel (Param channel) = if "#" `T.isPrefixOf` channel 
    then Just $ Channel channel 
    else Nothing
