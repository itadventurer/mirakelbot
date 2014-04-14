module Bot.NetIO where
import           Bot.Types
import           Control.Arrow
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Network
import           System.IO
import           System.Time
import           System.Time
import           Text.Printf

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
pubmsg msg = do chan <- asks $ botChan . botConfig
                privmsg chan msg

uptime :: Net String
uptime = do
    now <- io getClockTime
    zero <- asks starttime
    return .prettyTimeDiff $ diffClockTimes now zero

prettyTimeDiff :: TimeDiff -> String
prettyTimeDiff td =
    unwords $ map ( uncurry (++) . first show) $
        if null diffs then [(0,"s")] else diffs
        where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                        in (tot', (sec',typ):acc)
              metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
              diffs = filter ((/=0) . fst) $ reverse $snd $
                        foldl' merge (tdSec td, []) metrics
