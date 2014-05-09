{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.Handlers where
import           Control.Lens
import           Control.Monad.Reader
import           Data.Char
import           Data.Foldable
import           Data.Functor
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           MirakelBot.Types

-- | add one to the HandlerId
succHandlerId :: HandlerId -> HandlerId
succHandlerId (HandlerId i) = HandlerId $ 1+i

-- | Generates new unique HandelrId
generateHandlerId :: Irc HandlerId
generateHandlerId = lastHandlerId <%= succHandlerId

-- | Add a Handler to the Handler list
registerHandler :: Handler -> Irc HandlerId
registerHandler h = do
    i <- generateHandlerId
    botHandlers %= ((i,h) :)
    return i

-- | Register a new Handler which is called when the user calls a bang command
registerBangHandler :: Text -> (Text -> Handler) -> Irc HandlerId
registerBangHandler rawcmd h = do
    hotword <- view $ botConfig.botHotword
    let cmd = hotword <> rawcmd
    let newHandler =do 
                    msg <- view handlerMessage
                    case T.splitAt (T.length cmd) <$> msg ^? privateMessage of
                        Just (p,params) | p== cmd && (T.null params || isSpace (T.head params)) 
                            -> h (T.strip params)
                        _ -> return ()
    registerHandler newHandler
registerDirectMessageHandler :: Handler -> Irc HandlerId
registerDirectMessageHandler h = do
    bnick <- view (botConfig.botNick)
    let newHandler = do
            msg <- view handlerMessage
            case msg ^? privateDestination of
                Just (ToNick (Nick nick):_) | nick == bnick -> h
                _ -> case (bnick `T.isInfixOf`) <$> msg ^? privateMessage of
                    Just True -> h
                    _ -> return ()
    registerHandler newHandler

registerMentioningHandler :: Text -> Handler -> Irc HandlerId
registerMentioningHandler txt h = do
    let newHandler = do
            msg <- view handlerMessage
            case (txt `T.isInfixOf`) <$> msg ^? privateMessage of
                Just True -> h
                _ -> return ()
    registerHandler newHandler
-- | Removes a Handler from the Handler List
unregisterHandler :: HandlerId -> Irc ()
unregisterHandler hid = botHandlers %= filter (\ (i,_) -> i /= hid)

-- |
handleMessage :: Message -> Irc ()
handleMessage msg = do
    handlers <- use botHandlers
    env <- ask
    liftIO $ for_ handlers $ \(hid,h) -> runHandler (HandlerInfo msg env hid) h
    return ()
