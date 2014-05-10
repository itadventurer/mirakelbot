{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.Handlers where
import           Control.Lens
import           Control.Monad.Reader
import           Data.Foldable
import           MirakelBot.Types

-- | add one to the HandlerId
succHandlerId :: HandlerId -> HandlerId
succHandlerId (HandlerId i) = HandlerId $ 1+i

-- | Generates new unique HandelrId
generateHandlerId :: Irc HandlerId
generateHandlerId = undefined -- lastHandlerId <%= succHandlerId

-- | Add a Handler to the Handler list
registerHandler :: Handler () -> Irc HandlerId
registerHandler h = undefined {- do
    i <- generateHandlerId
    -- botHandlers %= ((i,h) :)
    return i -}

-- | Removes a Handler from the Handler List
unregisterHandler :: HandlerId -> Irc ()
unregisterHandler hid = undefined -- botHandlers %= filter (\ (i,_) -> i /= hid)

-- |
handleMessage :: Message -> Irc ()
handleMessage msg = undefined {-do
    handlers <- use botHandlers
    env <- ask
    liftIO $ for_ handlers $ \(hid,h) -> runHandler (HandlerInfo msg env hid) h
    return ()-}
