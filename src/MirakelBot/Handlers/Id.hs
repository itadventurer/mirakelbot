module MirakelBot.Handlers.Id where
import           Control.Monad
import           Data.Text               as T
import           MirakelBot.HandlerHelpers
import           MirakelBot.Message.Send
import           MirakelBot.Types

init :: Irc ()
init = void $ registerBangHandler (T.pack "id") handleId

handleId :: Text -> Handler ()
handleId = answer
