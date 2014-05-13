module MirakelBot.Handlers.Download where

import           Data.Maybe
import           Data.Tree.NTree.TypeDefs

import           Text.XML.HXT.Core

import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

import           Network.HTTP
import           Network.URI

openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail ""
    Just u  -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

get :: String -> IO (IOSArrow XmlTree (NTree XNode))
get url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withWarnings no,
    withEncodingErrors yes] (fromMaybe "" contents)

getXML :: String -> IO (IOSArrow XmlTree (NTree XNode))
getXML url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withWarnings no,
    withEncodingErrors yes] (fromMaybe "" contents)
