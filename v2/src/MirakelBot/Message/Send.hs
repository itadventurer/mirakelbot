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
send' h message = do
    liftIO $ BC.hPutStrLn h $ T.encodeUtf8 $ showt message
    liftIO $ putStrLn $ '>' : T.unpack (showt message)

