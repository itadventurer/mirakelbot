module Bot.Net where
import           Bot.Data
import           Bot.Handle
import           Bot.Parser
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Misc
import           Network
import           Prelude                       hiding (catch)
import           System.Exit
import           System.IO
import           System.Time
import           Text.ParserCombinators.Parsec (parse)
import           Text.Printf

connect :: Bot -> IO Bot
connect bot = notify $ do
        t <- getClockTime
        h <- connectTo (botServer bot) (botPort bot)
        hSetBuffering h NoBuffering
        return (bot {socket=h, starttime=t})
    where
        notify a = bracket_
            (printf "Connecting to %s ... " (botServer bot) >> hFlush stdout)
            (putStrLn "done.")
            a
run :: Net ()
run = do
    nick <- asks botNick
    chan <- asks botChan
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

-- Send a message out to the server we're connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf "> %s %s \n" s t

io :: IO a -> Net a
io = liftIO

privmsg :: String -> String -> Net ()
privmsg receiver message = write "PRIVMSG" (receiver ++ " :" ++ message)

pubmsg :: String -> Net ()
pubmsg msg = do chan <- asks botChan
                privmsg chan msg
