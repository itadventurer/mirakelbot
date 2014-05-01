module MirakelBot.Handlers.Id where
import MirakelBot.Types
import MirakelBot.Handlers
import MirakelBot.Message.Send
import Data.Text as T
import           Control.Monad.Reader

init :: Irc ()
init = registerBangHandler (T.pack "id") handleId >>
          return ()

handleId :: Text -> Handler
handleId txt msg = answer txt msg
handleId _ _ = return ()
