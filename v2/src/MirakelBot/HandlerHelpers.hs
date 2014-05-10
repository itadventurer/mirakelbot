{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.HandlerHelpers where
import           Control.Lens
import           Control.Monad.Reader
import           Data.Char
import           Data.Functor
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           MirakelBot.Types
import           MirakelBot.Handlers

getBang :: Text -> Handler (Maybe (Text,Text))
getBang rawcmd =do
    hotword <- view (botConfig.botHotword) <$> getBotEnv
    let cmd = hotword <> rawcmd
    msg <- getMessage
    case T.splitAt (T.length cmd) <$> msg ^? privateMessage of
                        Just (p,params) | p== cmd && (T.null params || isSpace (T.head params))
                            -> return $ Just (p,params)
                        _ -> return Nothing

-- | Register a new Handler which is called when the user calls a bang command
registerBangHandler :: Text -> (Text -> Handler ()) -> Irc HandlerId
registerBangHandler rawcmd h = do
    let newHandler = do bang <- getBang rawcmd
                        case bang of
                            Just (_,params) -> h (T.strip params)
                            _ -> return ()
    registerHandler newHandler

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
