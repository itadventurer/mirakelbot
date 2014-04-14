module Bot.Net where
import           Bot.Handle
import           Bot.NetIO
import           Bot.Parser
import           Bot.Types
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Network
import           System.Exit
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
    write "NICK" $ nick
    write "USER" (nick ++" 0 * :MirakelBot")
    write "JOIN" $ chan
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
        s <- init `fmap` io (hGetLine h)
        io (putStrLn s)
        let msg = parse parseMessage "(unknown)" s
        either (\_ -> modify $ updateMessage Nothing) saveMsg msg
        either printError handleCmd msg
    where
        printError = io . putStrLn . show
        handleCmd = maybe (return ()) evalCommand . interpretMessage
        saveMsg :: Message -> Net ()
        saveMsg msg = do
            modify $ updateMessage $ Just msg
        updateMessage mmsg state = state {lastMessage = mmsg}

