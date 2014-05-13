{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
-- | Talk with the bot
module MirakelBot.Handlers.Mirakel where

import           MirakelBot.HandlerHelpers
import           MirakelBot.Handlers
import           MirakelBot.Message.Send
import           MirakelBot.Types
import Data.Time.Clock
import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Monoid
import MirakelBot.Handlers.Download
import Text.XML.HXT.Core hiding (when)
import Data.Foldable
import Text.Read

masters :: [Nick]
masters = [Nick "azapps",Nick "weiznich"]

init :: Irc ()
init = do
    faqXml <- liftIO $ getFAQ
    ctime <- liftIO getCurrentTime
    let ntime = addUTCTime (fromIntegral (-5*600 :: Int)) ctime
    lastMentioning <- liftIO $ newMVar ntime
    _ <- registerMentioningHandler "Mirakel" $ handleMentioning lastMentioning
    _ <- registerMentioningHandler "mirakel" $ handleMentioning lastMentioning
    _ <- registerBangHandler "lastm" $ handleLastM lastMentioning
    _ <- registerBangHandler "faq" $ handleFAQ faqXml
    return ()

handleMentioning :: MVar UTCTime -> Handler ()
handleMentioning var = do
    lastMentioning <- liftIO $ readMVar var
    currentTime <- liftIO getCurrentTime
    liftIO $ modifyMVar_ var $ \_ -> return currentTime
    channel <- getCurrentChannel
    onlineUsers <- filterM (userIsOnline channel) masters
    when (diffUTCTime currentTime lastMentioning > fromIntegral (60*5 :: Int)) $
        if null onlineUsers then
            answer "I am the MirakelBot. My developers are currently offline. If your problem persist please write us an email to mirakel@azapps.de or join the #mirakel channel"
        else
            answer $ "I am the MirakelBot. If you have questions just ask " <> T.intercalate " or " (map getNick onlineUsers) <>  " or join the #mirakel channel"

handleLastM :: MVar UTCTime -> T.Text -> Handler ()
handleLastM var _ = do
    ntime <- liftIO $ readMVar var
    answer (T.pack $ show ntime)

handleFAQ :: FAQ -> T.Text -> Handler ()
handleFAQ faq params =
    case readMaybe $ T.unpack $ T.strip params of 
        Just a -> case (a-1) `safeGet` faq of
                Just ans -> answer $ snd ans
                _ -> traverse_ makeAnswer $ zip [1..] faq
        _ -> traverse_ makeAnswer $ zip [1..] faq
    where 
        makeAnswer (num,(q,_)) = answer $ T.unwords [T.pack $ show (num :: Int) ++ ".", q]
        safeGet :: Int -> [a] -> Maybe a
        safeGet _ []     = Nothing
        safeGet 0 (x:_)  = Just x
        safeGet i (_:xs) = safeGet (i-1) xs
    


type FAQ = [(T.Text,T.Text)]

getFAQ :: IO FAQ
getFAQ = do
    doc <- getXML "http://mirakel.azapps.de/faq.xml"
    runX $ doc >>> getFAQEntry

getFAQEntry :: ArrowXml a => a XmlTree (T.Text,T.Text)
getFAQEntry = proc x -> do
    item <- deep (hasName "item") -< x
    question <- getChildren >>> hasName "question" /> getText -< item
    manswer <- getChildren >>> hasName "answer" /> getText -< item
    returnA -< (T.pack question, T.pack manswer)
