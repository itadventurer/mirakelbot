{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module MirakelBot.Handlers where

import           Control.Lens
import           Control.Monad.Reader
import           MirakelBot.Types
import           Control.Applicative
import           Data.Unique
import           Control.Concurrent.MVar


runHandler :: HandlerInfo -> Handler () -> IO ()
runHandler i = flip runReaderT i . runHandler'

getMessage :: Handler Message
getMessage = Handler $ view handlerMessage

getBotEnv :: Handler BotEnv
getBotEnv = Handler $ view handlerEnv

getOwnId :: Handler HandlerId
getOwnId = Handler $ view handlerId

getUserList :: Handler UserList
getUserList = Handler $ view (handlerEnv.userlist) >>= liftIO . readMVar 

modifyUserList :: (UserList -> UserList) -> Handler ()
modifyUserList f = Handler $ do
    ul <- view $ handlerEnv.userlist
    liftIO $ modifyMVar_ ul $ return . f

-- | Generates new unique HandelrId
generateHandlerId :: Irc HandlerId
generateHandlerId = HandlerId <$> liftIO newUnique

-- | Add a Handler to the Handler list
registerHandler :: Handler () -> Irc HandlerId
registerHandler h = do
    i <- generateHandlerId
    mvar <- view handlers
    liftIO . modifyMVar_ mvar $ return . ((i,h) :)
    return i

-- | Removes a Handler from the Handler List
unregisterHandler :: HandlerId -> Irc ()
unregisterHandler hid = do
    mvar <- view handlers
    liftIO . modifyMVar_ mvar $ return . filter (\h -> fst h /= hid)

-- |
handleMessage :: Message -> Irc ()
handleMessage msg = do
    env <- ask
    hs <- liftIO . readMVar $ env^.handlers
    liftIO . forM_ hs $ \(hid, h) -> runHandler (HandlerInfo msg env hid) h
