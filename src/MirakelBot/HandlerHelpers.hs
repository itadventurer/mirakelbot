{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.HandlerHelpers where
import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Reader
import           Data.Char
import           Data.Functor
import qualified Data.Map             as M
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           MirakelBot.Handlers
import           MirakelBot.Types

getBang :: Text -> Handler (Maybe (Text,Text))
getBang rawcmd = do
    hotword <- view (botConfig.botHotword) <$> getBotEnv
    let cmd = hotword <> rawcmd
    msg <- getMessage
    case T.splitAt (T.length cmd) <$> msg ^? privateMessage of
                        Just (p,params) | p== cmd && (T.null params || isSpace (T.head params))
                            -> return $ Just (p,params)
                        _ -> return Nothing

-- | Register a new Handler which is called when the user calls a bang command
registerBangHandler :: Text                 -- ^ Bang command
                    -> (Text -> Handler ()) -- ^ Handler
                    -> Irc HandlerId        -- ^ HandlerId
registerBangHandler rawcmd h = do
    let newHandler = do bang <- getBang rawcmd
                        case bang of
                            Just (_,params) -> h (T.strip params)
                            _ -> return ()
    registerHandler newHandler

registerBangHandlerWithHelp :: Text                 -- ^ Bang command
                            -> Text                 -- ^ Help-Text
                            -> (Text -> Handler ()) -- ^ Handler
                            -> Irc HandlerId        -- ^ Return
registerBangHandlerWithHelp rawcmd helptext h = do
    help <- view botHelp
    liftIO $ modifyMVar_ help (\x -> return $ M.insert rawcmd helptext x)
    registerBangHandler rawcmd h

getHelp :: Handler (M.Map Text Text)
getHelp = Handler $ do
    mv <- view $ handlerEnv.botHelp
    liftIO $ readMVar mv

getHotword :: Handler (Text)
getHotword = Handler $ view $ handlerEnv.botConfig.botHotword


isDirectMessage :: Handler Bool
isDirectMessage = do
            bnick <- view (botConfig.botNick) <$> getBotEnv
            msg <- getMessage
            case msg ^? privateDestination of
                Just (ToNick (Nick nick):_) | nick == bnick -> return True
                _ -> case (bnick `T.isInfixOf`) <$> msg ^? privateMessage of
                    Just True -> return True
                    _ -> return False


registerDirectMessageHandler :: Handler () -> Irc HandlerId
registerDirectMessageHandler h = registerHandler $ isDirectMessage >>= flip when h

isMentioning :: Text -> Handler Bool
isMentioning txt = do
    msg <- getMessage
    case (txt `T.isInfixOf`) <$> msg ^? privateMessage of
            Just True -> return True
            _ -> return False

registerMentioningHandler :: Text -> Handler () -> Irc HandlerId
registerMentioningHandler txt h = registerHandler $ isMentioning txt >>= flip when h

getCurrentChannel :: Handler Channel
getCurrentChannel = do
    msg <- getMessage
    let toChannel (ToChannel c) = Just c
        toChannel _ = Nothing
    maybe mzero return $ (msg ^? privateDestination._head) >>= toChannel
