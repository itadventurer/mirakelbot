{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module MirakelBot.Internal where

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Map                as M
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import           System.IO
import           System.Time
import Network (PortID)
import Data.Unique
import Control.Applicative
import Data.String

-- | toText
class ShowT a where
    showt :: a -> Text


data UserMode = ModeNormal | ModeOperator | ModeVoice
    deriving (Show, Eq, Ord)

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
newtype Param   = Param { getParam :: Text } deriving (Eq,Show, IsString)
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


-- Message stuff
newtype User    = User { getUser :: Text } deriving (Show, Eq)
newtype Nick    = Nick { getNick :: Text } deriving (Show, Eq, Ord)
newtype Mask    = Mask { getMask :: Text } deriving (Show, Eq)
newtype Channel = Channel { getChannel :: Text } deriving (Show, Eq, Ord)

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

type UserList = M.Map Nick UserMode

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

data BotEnv = BotEnv {
      _botConfig :: BotConfig
    , _socket    :: Handle
    , _starttime :: ClockTime
    , _userlist  :: MVar (M.Map Channel UserList)
    , _handlers  :: MVar [(HandlerId, Handler ())]
    }


type Irc = ReaderT BotEnv IO

newtype HandlerId = HandlerId Unique
    deriving (Eq, Ord)

newtype Handler a = Handler { runHandler' :: MaybeT (ReaderT HandlerInfo IO) a }
    deriving (Monad, Applicative, Functor, MonadIO, Alternative, MonadPlus)

-- Handlers
data HandlerInfo = HandlerInfo {
      _handlerMessage :: Message
    , _handlerEnv     :: BotEnv
    , _handlerId      :: HandlerId
    }

makeLenses ''BotEnv

makeLenses ''HandlerInfo


