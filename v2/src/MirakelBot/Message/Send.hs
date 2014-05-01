module MirakelBot.Message.Send (send,send',answer) where
import MirakelBot.Types
import           Control.Monad.Reader
import           Control.Lens
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC

send :: Message -> Irc ()
send message = do
    env <- ask
    liftIO $ runHandler env (send' message)

answer :: T.Text -> Handler
answer txt msg=do
    let dest = getDest msg
    sendText txt [dest]

sendText :: T.Text -> [To] -> HandlerResult
sendText txt dest = do
    let msg = PrivateMessage Nothing dest txt
    send' msg
    

getDest :: Message -> To
getDest (PrivateMessage {_privateSender = Just sndr, _privateDestination = (dest:_)}) = 
    case dest of
        ToChannel _ -> dest
        ToUser _ -> sndr
        _ -> error "Wrong destination"
getDest _ = error "Wrong destination"


send' :: Handler
send' message = do
    h <- view socket
    liftIO $ BC.hPutStrLn h $ T.encodeUtf8 $ showt message
    liftIO $ putStrLn $ '>' : (T.unpack $ showt message)

