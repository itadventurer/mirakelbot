module Bot.Types where

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network              (PortID)
import           System.IO
import           System.Time

type Channel = String

data BotConfig = BotConfig {
      botServer  :: String
    , botPort    :: PortID
    , botChan    :: String
    , botNick    :: String
    , botHotword:: String
    , botMasters:: [String]
    }

data Bot = Bot {
      botConfig :: BotConfig
    , socket    :: Handle
    , starttime :: ClockTime
    }

data BotState = BotState {
      onlineUsers :: [User]
    , lastMessage :: Maybe TextMsg
    } deriving Show

data User = User {
      userName     :: String
    , userFullName :: String
    } deriving (Show, Eq)

data TextMsg = TextMsg {
      msgSender    :: User
    , msgDest      :: String
    , msgMessage   :: String
    } deriving Show


data Message =    Join User [Channel]
                | UserQuit User
                | Ping String
                | PrivMsg TextMsg
                | Other
                deriving Show

data Command = Pong String
             | AddUser User
             | DelUser User
             | Quit
             | HandleHotword {
                   hotHotword :: String
                 , hotParams  :: String
                 }
             | HandleMentioning String
             deriving Show

-- The Net monad
type Net = StateT BotState (ReaderT Bot IO)