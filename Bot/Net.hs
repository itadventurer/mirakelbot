module Bot.Net where
import           Bot.NetIO
import           Bot.Data
import           Bot.Handle
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
        users <- gets onlineUsers
        let cmd = parse parseMessage "(unknown)" s
        either (\_ -> return ()) (evalCommand . interpretMessage) cmd
    where
        forever a   = a >> forever a

        --clean       = drop 1 . dropWhile (/= ':') . drop 1
        clean (' ' : (':' : x)) = x
        clean (_:xs) = clean xs
        clean "" = ""

