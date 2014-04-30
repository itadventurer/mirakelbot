{-# LANGUAGE TemplateHaskell #-}
module MirakelBot.Types where
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Lens
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network              (PortID)
import           System.IO
import           System.Time
import Data.Monoid

-- | toText
class ShowT a where
    showt :: a -> Text

-- Message stuff
newtype User    = User { getUser :: Text } deriving (Show, Eq)
newtype Nick    = Nick { getNick :: Text } deriving (Show, Eq)
newtype Mask    = Mask { getMask :: Text } deriving (Show, Eq)
newtype Channel = Channel { getChannel :: Text } deriving (Show, Eq)
data Command    = PRIVMSG
                | Command Text
                | NumericCommand Int
                deriving (Eq,Show)
instance ShowT Command where
    showt (NumericCommand i) = T.pack $ show i
    showt (Command text) = text
    showt (PRIVMSG) = T.pack  "PRIVMSG"
newtype Param   = Param { getParam :: Text } deriving (Eq,Show)
instance ShowT Param where
    showt (Param param)
        | T.any (==' ') param = (T.pack ":") <> param
        | otherwise = param

newtype Host    = Host { getHost :: Text } deriving (Show, Eq)

data Prefix = ServerPrefix Text
            | NickPrefix {
                  prefixNick :: Nick
                , prefixUser :: (Maybe User)
                , prefixHost :: (Maybe Host)
                }
            deriving (Eq,Show)

instance ShowT Prefix where
    showt (ServerPrefix p) = p
    showt (NickPrefix (Nick nick) muser mhost) = nick 
        <> (printmuser muser) 
        <> (printmhost mhost)
        where
            printmuser :: Maybe User -> Text
            printmuser Nothing = T.pack ""
            printmuser (Just (User user)) = (T.pack "!") <> user
            printmhost :: Maybe Host -> Text
            printmhost Nothing = T.pack ""
            printmhost (Just (Host host)) = (T.pack "@") <> host

-- |<to>         ::= <channel> | <user> '@' <servername> | <nick> | <mask>
-- |Currently we are ignoring the mask because we don't need it
data To = ToChannel Channel
        | ToUser User
        | ToNick Nick
     -- | ToMask Mask
        deriving (Eq,Show)
instance ShowT To where
    showt (ToUser (User user)) = user
    showt (ToNick (Nick nick)) = nick
    showt (ToChannel (Channel channel)) = channel


-- |<message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
data Message = ServerMessage { -- [:Prefix] Command [Param1] .. [Param15]
                _serverPrefix  :: Maybe Prefix
              , _serverCommand :: Command
              , _serverParams  :: [Param]
            } |
            PrivateMessage {
                _privateSender      :: To
              , _privateDestination :: [To]
              , _privateMessage     :: Text
            }
            deriving (Eq,Show)

makeLenses ''Message

instance ShowT Message where
    showt (ServerMessage mprefix command params) = (printmprefix mprefix)
        <> (showt command) <> (T.pack " ") 
        <> (T.unwords $ map showt params)
        where 
            printmprefix Nothing = T.pack ""
            printmprefix (Just p) = (T.pack ":") <> (showt p) <> (T.pack " ")
    showt (PrivateMessage mprefix dest text) = (T.pack ":") <> (showt mprefix) 
        <> (T.pack " PRIVMSG ")
        <> (T.intercalate (T.pack "T") $ map showt dest) <> (T.pack " ")
        <> (showt $ Param text)
    

-- Handlers
newtype HandlerId = HandlerId { getHandlerId :: Int } 
    deriving (Show, Eq)
data HandlerCondition = PrefixCondition Text | InfixCondition Text
    deriving (Show, Eq)

-- Main Transformer
data BotConfig = BotConfig {
      _botServer  :: String
    , _botPort    :: PortID
    , _botChan    :: Text
    , _botNick    :: Text
    , _botRealName:: Text
    , _botHotword :: Text
    } deriving Show

makeLenses ''BotConfig

data BotEnv = BotEnv {
      _botConfig :: BotConfig
   , _socket    :: Handle
    , _starttime :: ClockTime
    } 

makeLenses ''BotEnv


type Irc = StateT BotState (ReaderT BotEnv IO)

type Handler = (Message -> HandlerResult)
type HandlerResult =ReaderT BotEnv IO ()

runHandler :: BotEnv -> ReaderT BotEnv IO () -> IO ()
runHandler = flip runReaderT

data BotState = BotState {
      _onlineUsers :: [User]
    , _lastMessage :: Maybe Message
    , _botMasters  :: [User]
    , _botHandlers :: [(HandlerId,Handler)]
    , _lastHandlerId :: HandlerId
    }

makeLenses ''BotState

