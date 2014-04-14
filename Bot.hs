import           Bot.Data
import Bot.Handle
import Bot.Parser
import           Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import           Bot.Net
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network
import           System.IO

config :: BotConfig
config = BotConfig {
      botServer  = "irc.freenode.net"
    , botPort    = PortNumber 6667
    , botChan    = "#mirakelbot"
    , botNick    = "mirakelbot"
    , botMasters = ["azapps"]
    , botHotword = "!"
    }

main :: IO ()
main = withSocketsDo $ bracket (connect config) disconnect loop
  where
    disconnect = hClose . socket
    initState  = BotState [] Nothing
    loop st    =  catch (runReaderT (runStateT run initState) st  >> return ())
                        (\(SomeException _) -> return ()) -- *** Control.Exception with base-4


