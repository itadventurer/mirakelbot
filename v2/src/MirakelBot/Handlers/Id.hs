module MirakelBot.Handlers.Id where
import MirakelBot.Types
import MirakelBot.Handlers
import MirakelBot.Message.Send
import Data.Text as T
import           Control.Monad.Reader

init :: Irc ()
init = registerBangHandler handleId (T.pack "id") >>
          return ()

handleId :: Handler
handleId msg@(PrivateMessage {_privateMessage = txt}) = do
    answer txt msg
handleId _ = return ()
