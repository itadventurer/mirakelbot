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
import           Text.Regex.Posix
import           Text.ParserCombinators.Parsec (parse)
import Bot.Data
import Bot.Parser
import Bot.Handle

server  = "irc.freenode.net"
port    = 6667
chan    = "#mirakelbot"
nick    = "mirakelbot"
masters = ["azapps"]
hotword = "!"

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    initState  = BotState []
    loop st    =  catch (runReaderT (runStateT run initState) st  >> return ())
                        (\(SomeException _) -> return ()) -- *** Control.Exception with base-4

connect :: IO Bot
connect = notify $ do
        t <- getClockTime
        h <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering h NoBuffering
        return (Bot h t)
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
pubmsg = privmsg chan

updateUserList :: BotState -> ([User]-> [User]) -> BotState
updateUserList state@(BotState {onlineUsers=list}) f=state {onlineUsers = f list}

isHot :: String -> String -> Bool
isHot command text = (hotword ++ command) `isPrefixOf` text
{-
eval :: Message -> Net ()
eval msg@(Message {messageText = text, messageAuthor = author})
    -- Hotwords
    | isHot "quit" text     = if author `elem` masters
                            then write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
                            else pubmsg "You are not my master!"
    | isHot "uptime" text   = uptime >>= pubmsg
    | isHot "id" text       = pubmsg (drop (2+ (length hotword)) text)
    | isHot "users" text    = do users <- gets onlineUsers
                                 pubmsg $ show users
    -- Mention Mirakel
    | "irakel" `isInfixOf` text  = handleMentioning msg
    -- Meta stuff
eval _                        = return () -- ignore everything else

handleMentioning :: Message -> Net ()
handleMentioning Message {messageText = text, messageAuthor = author, messageIsPrivate = isPrivate } = do
    privmsg author $ unwords ["Hey",author, "you've mentioned Mirakel!"]
    privmsg author "Can I help you?"
-}
uptime :: Net String
uptime = do
    now <- io getClockTime
    zero <- asks starttime
    return .prettyTimeDiff $ diffClockTimes now zero
