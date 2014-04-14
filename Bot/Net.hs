module Bot.Net where
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Misc
import           Network
import           Prelude              hiding (catch)
import           System.Exit
import           System.IO
import           System.Time
import           Text.Printf
import           Text.ParserCombinators.Parsec (parse)
import Bot.Data
import Bot.Handle
import Bot.Parser

connect :: IO Bot
connect config = notify $ do
        t <- getClockTime
        h <- connectTo (asks botServer) (asks botPort)
        hSetBuffering h NoBuffering
        return (Bot h t)
    where
        notify a = bracket_
            (printf "Connecting to %s ... " (botServer config) >> hFlush stdout)
            (putStrLn "done.")
            a
run :: Net ()
run = do
    write "NICK" $ asks botNick
    write "USER" ((asks botNick)++" 0 * :MirakelBot")
    write "JOIN" $ asks botChan
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
pubmsg = privmsg $ asks botChan
