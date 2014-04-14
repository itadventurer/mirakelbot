import           Bot.Data
import           Bot.Handle
import           Bot.Net
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
import           Text.Regex.Posix

config = Bot {
      botServer  = "irc.freenode.net"
    , botPort    = PortNumber 6667
    , botChan    = "#mirakelbot"
    , botNick    = "mirakelbot"
    , botMasters = ["azapps"]
    , botHotword = "!"
    }

main :: IO ()
main = bracket (connect config) disconnect loop
  where
    disconnect = hClose . socket
    initState  = BotState []
    loop st    =  catch (runReaderT (runStateT run initState) st  >> return ())
                        (\(SomeException _) -> return ()) -- *** Control.Exception with base-4


{-isHot :: String -> String -> Bool
isHot command text = (hotword ++ command) `isPrefixOf` text
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
