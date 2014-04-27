module Bot.Net where
import           Bot.Handle
import           Bot.Parser
import           Bot.Types
import Util.Irc
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network
import           System.IO
import           System.Time
import           Text.ParserCombinators.Parsec (parse)
import           Text.Printf

connect :: BotConfig -> IO Bot
connect config = notify $ do
        t <- getClockTime
        h <- connectTo (botServer config) (botPort config)
        hSetBuffering h NoBuffering
        return (Bot {socket=h, starttime=t, botConfig=config})
    where
        notify = bracket_
            (printf "Connecting to %s ... " (botServer config) >> hFlush stdout)
            (putStrLn "done.")

run :: Net ()
run = do
    nick <- asks $ botNick . botConfig
    chan <- asks $ botChan . botConfig
    writeRaw "NICK" [nick]
    writeRaw "USER" [nick, "0", "*", ":MirakelBot"]
    writeRaw "JOIN" [chan]
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
        rawMessage <- init `fmap` liftIO (hGetLine h)
        liftIO (putStrLn rawMessage)
        let msg = parse parseMessage "(unknown)" rawMessage
        either (\_ -> modify $ updateMessage Nothing) saveMsg msg
        either printError handleCmd msg
    where
        printError = liftIO . putStrLn . show
        handleCmd = maybe (return ()) evalCommand . interpretMessage
        saveMsg :: Message -> Net ()
        saveMsg (PrivMsg msg) = do
            modify $ updateMessage $ Just msg
        saveMsg _ = return ()
        updateMessage mmsg st = st {lastMessage = mmsg}

