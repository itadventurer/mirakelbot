import           Bot.Data
import           Bot.Handle
import           Bot.Net
import           Bot.Parser
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Network
import           Prelude                       hiding (catch)
import           System.Exit
import           System.IO
import           System.Time
import           Text.ParserCombinators.Parsec (parse)
import           Text.Printf
import           Text.Regex.Posix

config = BotConfig {
      botServer  = "irc.freenode.net"
    , botPort    = PortNumber 6667
    , botChan    = "#mirakelbot"
    , botNick    = "mirakelbot"
    , botMasters = ["azapps"]
    , botHotword = "!"
    }

main :: IO ()
main = bracket (connect config) disconnect loop
  where
    disconnect = hClose . socket
    initState  = BotState []
    loop st    =  catch (runReaderT (runStateT run initState) st  >> return ())
                        (\(SomeException _) -> return ()) -- *** Control.Exception with base-4


