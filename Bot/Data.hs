module Bot.Data where

import           System.IO
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           System.Time
import           Network (PortNumber)

type Channel = String

data BotConfig = BotConfig {
      botServer :: String
    , botPort   :: PortNumber
    , botChan   :: String
    , botNick   :: String
    }

data Bot = Bot {
    socket    :: Handle,
    starttime :: ClockTime
    }

data BotState = BotState {
    onlineUsers :: [User]
    } deriving Show

data User = User {
      userName :: String
    , userFullName :: String
    } deriving (Show, Eq)
{-
data Message = Message {
      messageAuthor    :: String
    , messageReceiver  :: String
    , messageIsPrivate :: Bool
    , messageText      :: String
    } deriving Show
-}
data Message =    Join User [Channel]
                | UserQuit User
                | Ping String
                | PrivMsg User String String
                | Other
                deriving Show

data Command = Pong String
             | UpdateUser ([User] -> [User])
             | Quit
             | DoNothing

-- The Net monad
type Net = StateT BotState (ReaderT Bot IO)
