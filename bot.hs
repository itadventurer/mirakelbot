import Data.List
import Network
import System.IO
import System.Exit
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Text.Regex.Posix
import Prelude hiding (catch)
import System.Time
import Misc

server  = "irc.freenode.net"
port    = 6667
chan    = "#mirakelbot"
nick    = "mirakelbot"
masters = ["azapps"]
hotword = "!"

data Bot = Bot { 
    socket :: Handle, 
    starttime :: ClockTime,
    onlineUsers :: [String]}
data Message = Message { 
    messageAuthor :: String, 
    messageReceiver :: String, 
    messageIsPrivate :: Bool, 
    messageText :: String 
    } deriving Show

-- The Net monad
type Net = ReaderT Bot IO


main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    =  catch (runReaderT run st) (\(SomeException _) -> return ()) -- *** Control.Exception with base-4

connect :: IO Bot
connect = notify $ do
        t <- getClockTime
        h <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering h NoBuffering
        return (Bot h t [])
    where
        notify a = bracket_
            (printf "Connecting to %s ... " server >> hFlush stdout)
            (putStrLn "done.")
            a
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :MirakelBot")
    write "JOIN" chan
    asks socket >>= listen


listen :: Handle -> Net ()
listen h = forever $ do
        s <- init `fmap` io (hGetLine h)
        io (putStrLn s)
        let msg = parseMessage s
        if ping s then pong s else maybe (return ()) eval msg
    where
        forever a   = a >> forever a

        --clean       = drop 1 . dropWhile (/= ':') . drop 1
        clean (' ' : (':' : x)) = x
        clean (_:xs) = clean xs
        clean "" = ""

        ping x      = "PING :" `isPrefixOf` x
        pong x      = write "PONG" (':' : drop 6 x)

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
pubmsg = privmsg chan

parseMessage :: String -> Maybe Message
parseMessage msg = if msg=~pattern then Just (Message author receiver isPrivate text) else Nothing
    where 
        pattern = ":(.*)!(.*) PRIVMSG (.*) :(.*)$"
        toArr msg = msg =~ pattern :: [[String]]
        [[_,author,_,receiver,text]] = toArr msg
        isPrivate = not ("#" `isPrefixOf` receiver)

isHot :: String -> String -> Bool
isHot command text = (hotword ++ command) `isPrefixOf` text

eval :: Message -> Net ()
eval msg@(Message {messageText = text, messageAuthor = author})
    | isHot "quit" text     = if author `elem` masters 
                            then write "QUIT" ":Exiting" >> io (exitWith ExitSuccess) 
                            else pubmsg "You are not my master!"
    | isHot "uptime" text   = uptime >>= pubmsg
    | isHot "id" text       = pubmsg (drop (2+ (length hotword)) text)
    | "irakel" `isInfixOf` text  = handleMentioning msg
eval _                        = return () -- ignore everything else

handleMentioning :: Message -> Net ()
handleMentioning Message {messageText = text, messageAuthor = author, messageIsPrivate = isPrivate } = do
    privmsg author $ unwords ["Hey",author, "you've mentioned Mirakel!"]
    privmsg author "Can I help you?"

uptime :: Net String
uptime = do
    now <- io getClockTime
    zero <- asks starttime
    return .prettyTimeDiff $ diffClockTimes now zero
