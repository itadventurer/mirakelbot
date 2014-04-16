module Main where 
import           Bot.Bot
import           Bot.Types
import Util.Irc
import           System.Exit
import           Control.Monad.Reader
import           Control.Monad.State
import           Network
import           Control.Arrow
import           Data.List
import           System.Time
import          Options.Applicative
import Data.Monoid

myHandlers :: [BotHandler]
myHandlers =[
         (HotwordPrefix "quit", quit)
       , (HotwordInfix "irakel", handleMention)
       , (HotwordPrefix "id", answerId)
       , (HotwordPrefix "uptime", answerUptime)
       , (HotwordPrefix "users", answerUsers)
    ]

main :: IO ()
main = do
    -- Parse
    cfg <- execParser opts
    runBot cfg

    where
        opts = info (helper <*> parser) mempty
        parser = BotConfig
                    <$> strOption
                        ( short 's'
                       <> long "server"
                       <> metavar "SERVER"
                       <> help "IRC Server where I should connect to" )
                    <*> (PortNumber . fromIntegral <$> option
                        ( short 'p'
                       <> long "port"
                       <> metavar "PORT"
                       <> value 6667
                       <> help "Port" ))
                    <*> strOption
                        ( short 'c'
                       <> long "chan"
                       <> metavar "CHANNEL"
                       <> help "Channel" )
                    <*> strOption
                        ( short 'n'
                       <> long "nick"
                       <> metavar "NICK"
                       <> help "Nick name" )
                    <*> strOption
                        ( long "hotword"
                       <> metavar "HOTWORD"
                       <> help "The prefix hotword" )
                    <*> many (option
                        ( short 'm'
                       <> long "masters"
                       <> metavar "MASTERS"
                       <> help "List of master users" ))
                    <*> pure myHandlers

quit :: Hook
quit TextMsg { msgSender = sender}  _ = do
    masters <- asks $ botMasters . botConfig
    if (userName sender) `elem` masters
    then
        writeRaw "QUIT" [":Exiting"] >> liftIO (exitWith ExitSuccess)
    else
        answer "You are not my master!"

handleMention :: Hook
handleMention TextMsg { msgSender = sender} _ = do
    answer $ unwords ["Hello", userName sender,"please tell me what you want to know"]

getWord :: Hotword -> String
getWord (HotwordPrefix x) = x
getWord (HotwordInfix x) = x

answerId :: Hook
answerId TextMsg { msgMessage = msg} hot = do
    prefixHotword <- asks $ botHotword . botConfig
    answer $ text prefixHotword msg
    where
        text :: String -> String -> String
        text p message = snd $ splitAt ((length p) + (length $ getWord hot)) message

answerUptime :: Hook
answerUptime _ _ = do
    now <- liftIO getClockTime
    zero <- asks starttime
    answer $ prettyTimeDiff $ diffClockTimes now zero

prettyTimeDiff :: TimeDiff -> String
prettyTimeDiff td =
    unwords $ map ( uncurry (++) . first show) $
        if null diffs then [(0,"s")] else diffs
        where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                        in (tot', (sec',typ):acc)
              metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
              diffs = filter ((/=0) . fst) $ reverse $snd $
                        foldl' merge (tdSec td, []) metrics

answerUsers :: Hook
answerUsers _ _ = do
    users <- gets onlineUsers
    answer $ show $ map userName users

