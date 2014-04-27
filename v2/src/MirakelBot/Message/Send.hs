module MirakelBot.Message.Send (send) where
import MirakelBot.Types
import Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Lens
import           System.IO

send :: Message -> Irc ()
send message = do
    h <- view socket
    liftIO $ hPrint h $ show message
    liftIO $ putStrLn $ '>' : (show message)
