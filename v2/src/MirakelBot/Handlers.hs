module MirakelBot.Handlers where
import MirakelBot.Types
import Control.Lens
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Data.Foldable
import Control.Monad.Reader

-- | add one to the HandlerId
succHandlerId :: HandlerId -> HandlerId
succHandlerId (HandlerId i) = HandlerId $ 1+i

-- | Generates new unique HandelrId
generateHandlerId :: Irc HandlerId
generateHandlerId = lastHandlerId <%= succHandlerId

-- | Add a Handler to the Handler list
registerHandler :: Handler -> Irc (HandlerId)
registerHandler h = do
    i <- generateHandlerId
    botHandlers %= ((i,h) :)
    return i

isPrefixOfMsg :: Text -> Message -> Bool
isPrefixOfMsg prefix (PrivateMessage {_privateMessage = txt}) = prefix `T.isPrefixOf` txt
isPrefixOfMsg _ _ = False

-- | Register a new Handler which is called when the user calls a bang command
registerBangHandler :: Handler -> Text -> Irc (HandlerId)
registerBangHandler h rawcmd = do
    cfg <- view botConfig
    let hotword = view botHotword cfg
    let cmd = hotword <> rawcmd
    let newHandler = \msg ->
                       if cmd `isPrefixOfMsg` msg then h msg
                       else return ()
    i <- generateHandlerId
    botHandlers %= ((i,newHandler) :)
    return i

-- | Removes a Handler from the Handler List
unregisterHandler :: HandlerId -> Irc ()
unregisterHandler hid = botHandlers %= filter (\ (i,_) -> i /= hid)

-- | 
handleMessage :: Message -> Irc ()
handleMessage msg = do
    handlers <- use botHandlers
    env <- ask
    liftIO $ for_ handlers $ \(_,h) -> runHandler env $ h msg
    return ()
