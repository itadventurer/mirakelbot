module Bot.NetIO where
import           Bot.Types
import           Control.Arrow
import           Control.Monad.Reader
import           Data.List
import           System.Time

uptime :: Net String
uptime = do
    now <- liftIO getClockTime
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

