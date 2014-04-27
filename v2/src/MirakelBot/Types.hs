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


{-
<command>  ::= <letter> { <letter> } | <number> <number> <number>
<SPACE>    ::= ' ' { ' ' }
<params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]

<middle>   ::= <Any *non-empty* sequence of octets not including SPACE
               or NUL or CR or LF, the first of which may not be ':'>
<trailing> ::= <Any, possibly *empty*, sequence of octets not including
                 NUL or CR or LF>

<crlf>     ::= CR LF

----

   <channel>    ::= ('#' | '&') <chstring>
   <servername> ::= <host>
   <host>       ::= see RFC 952 [DNS:4] for details on allowed hostnames
   <nick>       ::= <letter> { <letter> | <number> | <special> }
   <mask>       ::= ('#' | '$') <chstring>
   <chstring>   ::= <any 8bit code except SPACE, BELL, NUL, CR, LF and
                     comma (',')>

   Other parameter syntaxes are:

   <user>       ::= <nonwhite> { <nonwhite> }
   <letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
   <number>     ::= '0' ... '9'
   <special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'


 -}

-- Message stuff
newtype User    = User { getUser :: Text } deriving (Show, Eq)
newtype Nick    = Nick { getNick :: Text } deriving (Show, Eq)
newtype Mask    = Mask { getMask :: Text } deriving (Show, Eq)
newtype Channel = Channel { getChannel :: Text } deriving (Show, Eq)
data Command    = Command Text
                | NumericCommand Int
                deriving (Eq)
instance Show Command where
    show (NumericCommand i) = show i
    show (Command text) = T.unpack text
newtype Param   = Param { getParam :: Text } deriving (Eq)
instance Show Param where
    show (Param tparam) = let param = T.unpack tparam in
                            if ' ' `elem` param then ':' : param else param
newtype Host    = Host { getHost :: Text } deriving (Show, Eq)

data Prefix = ServerPrefix Text
            | NickPrefix Nick (Maybe User) (Maybe Host)
            deriving (Eq)

instance Show Prefix where
    show (ServerPrefix p) = T.unpack p
    show (NickPrefix (Nick nick) muser mhost) = (T.unpack nick) ++ (printmuser muser) ++ (printmhost mhost)
        where
            printmuser Nothing = ""
            printmuser (Just (User user)) = '!' : (T.unpack user)
            printmhost Nothing = ""
            printmhost (Just (Host host)) = '@' : (T.unpack host)

-- |<target>     ::= <to> [ "," <target> ]
newtype Target = Target { getTarget :: [To] } deriving (Show, Eq)

-- |<to>         ::= <channel> | <user> '@' <servername> | <nick> | <mask>
-- |Currently we are ignoring the mask because we don't need it
data To = ToChannel Channel
        | ToUser User
        | ToNick Nick
     -- | ToMask Mask
        deriving (Eq, Show)


-- |<message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
data Message = ServerMessage { -- [:Prefix] Command [Param1] .. [Param15]
                serverPrefix  :: Maybe Prefix
              , serverCommand :: Command
              , serverParams  :: [Param]
            } |
            PrivateMessage {
                privateSender      :: Prefix
              , privateDestination :: To
              , privateMessage     :: Text
            }
            deriving (Eq)

instance Show Message where
    show (ServerMessage mprefix command params) = (printmprefix mprefix) ++ (show command) ++ " " ++ (unwords $ map show params)
        where
            printmprefix Nothing = ""
            printmprefix (Just p) = ':' : (show p ++ " ")

-- Handlers
newtype HandlerId = HandlerId { getHandlerId :: Int } 
    deriving (Show, Eq)
data HandlerCondition = PrefixCondition Text | InfixCondition Text
    deriving (Show, Eq)
data HandlerAction = SimpleAction (String -> String)
data Handler = Handler {
             handlerCondition :: HandlerCondition
             , handlerAction :: HandlerAction
             }

instance Show Handler where
    show h = "Handler (condition: " ++ (show $ handlerCondition h ) ++ ")"

-- Main Transformer
data BotConfig = BotConfig {
      _botServer  :: String
    , _botPort    :: PortID
    , _botChan    :: Text
    , _botNick    :: Text
    , _botHotword :: Text
    } deriving Show

makeLenses ''BotConfig

data BotEnv = BotEnv {
      _botConfig :: BotConfig
    , _socket    :: Handle
    , _starttime :: ClockTime
    } 

makeLenses ''BotEnv


data BotState = BotState {
      _onlineUsers :: [User]
    , _lastMessage :: Maybe Message
    , _botMasters  :: [User]
    , _botHandlers :: [(HandlerId,Handler)]
    , _lastHandlerId :: HandlerId
    } deriving Show

makeLenses ''BotState

type Irc = StateT BotState (ReaderT BotEnv IO)
