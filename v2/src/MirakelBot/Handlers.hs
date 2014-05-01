{-# LANGUAGE OverloadedStrings #-}
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

splitMsgAt :: Int -> Message -> Maybe (Text,Text)
splitMsgAt at (PrivateMessage {_privateMessage = txt}) = Just $ T.splitAt at txt
splitMsgAt _ _ = Nothing


-- | Register a new Handler which is called when the user calls a bang command
registerBangHandler :: Text -> (Text -> Handler) -> Irc (HandlerId)
registerBangHandler rawcmd h = do
    cfg <- view botConfig
    let hotword = view botHotword cfg
    let cmd = hotword <> rawcmd
    let newHandler = msg -> case splitMsgAt (T.length cmd) msg of
                        Just (p,params) | p== cmd -> h params msg
                        _ -> return ()
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
