module Bot.Bot where
import           Bot.Net
import           Bot.Types
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network
import           System.IO

runBot :: BotConfig -> IO ()
runBot config = withSocketsDo $ bracket (connect config) disconnect mloop
  where
    disconnect = hClose . socket
    initState  = BotState [] Nothing
    mloop st    =  catch (runReaderT (runStateT run initState) st  >> return ())
                        (\(SomeException _) -> return ()) -- *** Control.Exception with base-4
