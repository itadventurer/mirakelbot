module MirakelBot.Net where
import MirakelBot.Types
import MirakelBot.Handlers
import           Control.Exception
import           Control.Monad.Reader
import           Network
import           System.IO
import           System.Time
import           Text.Printf
import MirakelBot.Message.Send
import Control.Lens
import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec (parseOnly)
import MirakelBot.Message.Receive
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Monoid
connect :: BotConfig -> IO BotEnv
connect config = notify $ do
        startTime <- getClockTime
        h <- connectTo (view botServer config) (view botPort config)
        hSetBuffering h NoBuffering
        return (BotEnv config h startTime)
    where
        notify = bracket_
            (printf "Connecting to %s ... " (view botServer config) >> hFlush stdout)
            (putStrLn "done.")

run :: [Irc ()] -> Irc ()
run handlers = do
    sequence_ handlers
    cfg <- view botConfig
    let nick = view botNick cfg
    let chan = view botChan cfg
    let real = view botRealName cfg
    send $ ServerMessage Nothing (Command $ T.pack "NICK") (Param <$> [nick])
    send $ ServerMessage Nothing (Command $ T.pack "USER") (Param <$> [nick, T.pack "0", T.pack "*", (T.pack ":") <> real])
    send $ ServerMessage Nothing (Command $ T.pack "JOIN") (Param <$> [chan])
    asks _socket >>= listen

listen :: Handle -> Irc ()
listen h = forever $ do
        rawMessage <- B.init `fmap` liftIO (B.hGetLine h)
        liftIO $ putStrLn $ BC.unpack rawMessage
        let msg = parseOnly parseMessage rawMessage
        either handleError handleSucc msg
    where
        printError = liftIO . putStrLn . show

        handleError :: String -> Irc ()
        handleError err= do
            lastMessage .= Nothing
            printError err

        handleSucc :: Message -> Irc ()
        handleSucc msg = do
            lastMessage .= Just msg
            handleMessage msg
