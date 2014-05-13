module MirakelBot.Message.Send (send,sendText,send',answer) where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           MirakelBot.Types
import           MirakelBot.Handlers
import           System.IO

send :: Message -> Handler ()
send message = do
    env <- getBotEnv
    let h = view socket env
    liftIO $ send' h message

answer :: T.Text -> Handler ()
answer txt=do
    msg <- getMessage
    let dest = getDest msg
    sendText txt [dest]

sendText :: T.Text -> [To] -> Handler ()
sendText txt dest = do
    let msg = PrivateMessage Nothing dest txt
    env <- getBotEnv
    let h = view socket env
    liftIO $ send' h msg


getDest :: Message -> To
getDest (PrivateMessage {_privateSender = Just sndr, _privateDestination = (dest:_)}) =
    case dest of
        ToChannel _ -> dest
        _ -> ToNick sndr
        --_ -> error "Wrong destination"
getDest _ = error "Wrong destination"


send' :: Handle -> Message -> IO ()
send' h message@(PrivateMessage {_privateMessage = txt}) = 
    mapM_ (\l -> sendOneLiner h message {_privateMessage = l}) $ T.lines txt
send' h message = sendOneLiner h message

sendOneLiner :: Handle -> Message -> IO ()
sendOneLiner h message = do
    let msgT= showt message
    liftIO $ BC.hPutStrLn h $ T.encodeUtf8 msgT
    liftIO $ putStrLn $ '>' : T.unpack msgT
