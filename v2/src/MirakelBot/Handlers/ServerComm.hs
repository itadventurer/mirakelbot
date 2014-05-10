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

commHandler :: [Handler ()]
commHandler = [handlePing,handleNameReply]

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
