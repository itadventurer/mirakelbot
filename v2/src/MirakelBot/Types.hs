{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module MirakelBot.Types where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                as M
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Network                 (PortID)
import           System.IO
import           System.Time

-- | toText
class ShowT a where
    showt :: a -> Text

-- Message stuff
newtype User    = User { getUser :: Text } deriving (Show, Eq)
newtype Nick    = Nick { getNick :: Text } deriving (Show, Eq)
newtype Mask    = Mask { getMask :: Text } deriving (Show, Eq)
newtype Channel = Channel { getChannel :: Text } deriving (Show, Eq)

data UserMode = ModeNormal | ModeOperator

data Command    = PRIVMSG
                | PING
                | PONG
                | Command Text
                | NumericCommand Int
                deriving (Eq,Show,Read)
instance ShowT Command where
    showt (NumericCommand i) = T.pack $ show i
    showt (Command text) = text
    showt (PRIVMSG) = "PRIVMSG"
    showt cmd = T.pack $ show cmd
newtype Param   = Param { getParam :: Text } deriving (Eq,Show)
instance ShowT Param where
    showt (Param param)
        | T.any (==' ') param = ":" <> param
        | otherwise = param

newtype Host    = Host { getHost :: Text } deriving (Show, Eq)

data Prefix = ServerPrefix Text
            | NickPrefix {
                  prefixNick :: Nick
                , prefixUser :: Maybe User
                , prefixHost :: Maybe Host
                }
            deriving (Eq,Show)

instance ShowT Prefix where
    showt (ServerPrefix p) = p
    showt (NickPrefix (Nick nick) muser mhost) = nick
        <> printmuser muser
        <> printmhost mhost
        where
            printmuser :: Maybe User -> Text
            printmuser Nothing = ""
            printmuser (Just (User user)) = "!" <> user
            printmhost :: Maybe Host -> Text
            printmhost Nothing = T.pack ""
            printmhost (Just (Host host)) = "@" <> host

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
                _privateSender      :: Maybe To
              , _privateDestination :: [To]
              , _privateMessage     :: Text
            }
            deriving (Eq,Show)

makeLenses ''Message

instance ShowT Message where
    showt (ServerMessage mprefix command params) = printmprefix mprefix
        <> showt command <> " "
        <> T.unwords (map showt params)
    showt (PrivateMessage mprefix dest text) = printmprefix mprefix
        <> " PRIVMSG "
        <> T.intercalate "T" (map showt dest) <> " "
        <> showt (Param text)
printmprefix :: ShowT a => Maybe a -> Text
printmprefix Nothing = T.pack ""
printmprefix (Just p) = ":" <> showt p <> " "


-- Handlers
newtype HandlerId = HandlerId { getHandlerId :: Int }
    deriving (Show, Eq)
data HandlerCondition = PrefixCondition Text | InfixCondition Text
    deriving (Show, Eq)

-- Main Transformer
data BotConfig = BotConfig {
      _botServer   :: String
    , _botPort     :: PortID
    , _botChan     :: Text
    , _botNick     :: Text
    , _botRealName:: Text
    , _botHotword  :: Text
    } deriving Show

makeLenses ''BotConfig

type UserList = M.Map Nick UserMode
data BotEnv = BotEnv {
      _botConfig :: BotConfig
    , _socket    :: Handle
    , _starttime :: ClockTime
    , _userlist  :: MVar UserList
    }

makeLenses ''BotEnv


data HandlerInfo = HandlerInfo {
      _handlerMessage :: Message
    , _handlerEnv     :: BotEnv
    , _handlerId      :: HandlerId
    }
makeLenses ''HandlerInfo
type Irc = StateT BotState (ReaderT BotEnv IO)


newtype Handler a = Handler { runHandler' :: ReaderT HandlerInfo IO a }
    deriving (Monad, Applicative, Functor, MonadIO)

runHandler :: HandlerInfo -> Handler () -> IO ()
runHandler i = flip runReaderT i . runHandler'

getMessage :: Handler Message
getMessage = Handler $ view handlerMessage

getBotEnv :: Handler BotEnv
getBotEnv = Handler $ view handlerEnv

getOwnId :: Handler HandlerId
getOwnId = Handler $ view handlerId

getUserList :: Handler UserList
getUserList = Handler $ view (handlerEnv.userlist) >>= liftIO . readMVar 

modifyUserList :: (UserList -> UserList) -> Handler ()
modifyUserList f = Handler $ do
    ul <- view $ handlerEnv.userlist
    liftIO $ modifyMVar_ ul $ return . f

data BotState = BotState {
      _onlineUsers   :: [User]
    , _lastMessage   :: Maybe Message
    , _botMasters    :: [User]
    , _botHandlers   :: [(HandlerId,Handler ())]
    , _lastHandlerId :: HandlerId
    }

makeLenses ''BotState

