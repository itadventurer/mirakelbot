module MirakelBot.Handlers.ServerComm where
import           Data.Foldable
import           MirakelBot.Handlers
import           MirakelBot.Message.Send
import           MirakelBot.Types

commHandler :: [Handler]
commHandler = [handlePing]

init :: Irc ()
init = do
    traverse_ registerHandler commHandler
    return ()

handlePing :: Handler
handlePing msg@(ServerMessage {_serverCommand = PING, _serverParams = p}) = send' $ msg {
                                              _serverPrefix = Nothing
                                            , _serverCommand = PONG
                                            , _serverParams = p
                                            }
handlePing _ = return ()
