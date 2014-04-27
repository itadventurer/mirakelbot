module MirakelBot.Handlers where
import MirakelBot.Types
import           Control.Monad.Reader
import           Control.Monad.State
import Control.Lens
import Control.Applicative

-- | add one to the HandlerId
succHandlerId :: HandlerId -> HandlerId
succHandlerId (HandlerId i) = HandlerId $ 1+i

-- | Generates new unique HandelrId
generateHandlerId :: Irc HandlerId
generateHandlerId = lastHandlerId <%= succHandlerId

-- | Add a Handler to the Handler list
registerHandler :: Handler -> Irc (HandlerId)
registerHandler h = do
    id <- generateHandlerId
    botHandlers %= ((id,h) :)
    return id

-- | Removes a Handler from the Handler List
unregisterHandler :: HandlerId -> Irc ()
unregisterHandler hid = botHandlers %= filter (\ (id,_) -> id /= hid)
