module Util.Irc where
import           Bot.Types
import           Control.Monad.Reader
import           Control.Monad.State
import           Text.Printf

getDest :: TextMsg  -- ^ Message
        -> String   -- ^ Channel or nick
getDest TextMsg {msgDest = channel@('#' : _)} = channel
getDest TextMsg {msgSender = sender} = userName sender

writeRaw :: String      -- ^ IRC command
         -> [String]    -- ^ Parameters
         -> Net ()
writeRaw cmd params = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" cmd (unwords params)
    liftIO $ printf "> %s %s \n" cmd (unwords params)

cutMsg :: String -> String
cutMsg msg | length msg <= 400 = msg
       | otherwise = (take 399 msg) ++ "…"
 
writeTo :: String   -- ^ Channel or Nick
        -> String   -- ^ Message
        -> Net ()   -- ^ Result
writeTo dest msg = writeRaw "PRIVMSG" [dest, cutMsg msg]
answer :: String    -- ^ Message
       -> Net ()    -- ^ Result
answer msg = do
    lastmsg <- gets lastMessage
    maybe (return ()) writeTo' lastmsg
    where  
        writeTo' lmsg = writeTo (getDest lmsg) msg
