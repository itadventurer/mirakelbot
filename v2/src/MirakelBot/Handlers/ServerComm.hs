{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.Handlers.ServerComm where

import           Data.Foldable
import           MirakelBot.Handlers
import           MirakelBot.Message.Send
import           MirakelBot.Types
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

commHandler :: [Handler ()]
commHandler = [handlePing]

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
        ServerMessage {_serverCommand = NumericCommand 353, _serverParams = [Param channelname, Param usernames]} -> do
            let channel = Channel channelname
            let users = M.fromList $ map processUser $ T.words usernames
            modifyUserList channel $ M.union users
            sendText "GetList" [ToChannel $ Channel "#mirakelbot"]
            where
                processUser :: Text -> (Nick, UserMode)
                processUser user 
                    | "@" `T.isInfixOf` user = (Nick user, ModeOperator)
                    | otherwise = (Nick user, ModeNormal)
        _ -> return ()
