module MirakelBot.Handlers.ServerComm where
import           Control.Lens
import           Data.Foldable
import           MirakelBot.Handlers
import           MirakelBot.Message.Send
import           MirakelBot.Types

commHandler :: [Handler ()]
commHandler = [handlePing]

init :: Irc ()
init = do
    traverse_ registerHandler commHandler
    return ()

handlePing :: Handler ()
handlePing =do 
            msg <- view handlerMessage
            case msg of
                ServerMessage {_serverCommand = PING, _serverParams = p} -> 
                    send msg {
                          _serverPrefix = Nothing
                        , _serverCommand = PONG
                        , _serverParams = p
                        }
                _ -> return ()
