{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Handler.Home where

import Import
import Language.Haskell.TH
import Yesod.EventSource
import Network.Wai.EventSource.EventStream (ServerEvent (ServerEvent))
import Blaze.ByteString.Builder.Char.Utf8 (fromShow, fromText)
import Blaze.ByteString.Builder (Builder)

import Control.Concurrent (threadDelay)

--import SharedTypes

data EventSourceState = EventSourceState Int

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Inter"
        addScript (StaticR js_Three_js)
        addScript (StaticR js_ColladaLoader_js)
        $(widgetFile "homepage")
        $(fayFile' (ConE 'StaticR) "Client")

getEventR :: Handler TypedContent
getEventR = do
    --master <- getYesod
    let source_state = EventSourceState 0
    ioToRepEventSource source_state ioEventSourceFunc

ioEventSourceFunc :: EventSourcePolyfill -> EventSourceState -> IO ([ServerEvent], EventSourceState)
ioEventSourceFunc _polyfill (EventSourceState s) = do
    sleepSeconds 1
    return ([mkLogEvent [fromText "ServerTick ", fromShow s]], EventSourceState (s+1))

mkLogEvent :: [Builder] -> ServerEvent
mkLogEvent content = ServerEvent (Just $ fromText "log") Nothing content

sleepSeconds :: Int -> IO ()
sleepSeconds n = threadDelay $ n * 1000000
