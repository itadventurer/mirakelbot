module MirakelBot where
import MirakelBot.Types
import MirakelBot.Handlers
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network
import           System.IO
import           System.Time
import           Text.ParserCombinators.Parsec (parse)
import           Text.Printf
import MirakelBot.Message.Send
import Control.Lens
import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec (parseOnly)
import MirakelBot.Message.Receive
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
{-
16:32:38) Richard: im prinzip ist die MVar nicht das entscheidende
(16:32:53) Richard: du brauchst auf generellste ebene folgende funktion:
(16:33:24) Richard: registerCallback :: (Event -> Net ()) -> Net ()
(16:33:36) Richard: wobei ich grade überlege, wie du das unregister am besten machst, aber ist erstmal egal
(16:34:01) Richard: und diese funktion fügt quasi dynamisch einen neuen handler hinzu
(16:34:21) Richard: jetzt brauchst du für jeden handler noch eine initialisierungfunktion vom typ Net ()
(16:34:38) Richard: und die kann dann registerCallback aufrufen
(16:34:40) Richard: z.B. so
(16:35:36) Richard: pingInit :: Net ()
pingInit = registerCallback $ \evt -> do
	args <- getBangCommand evt "!ping"
	write args
(16:35:39) Richard: also so in der art
(16:35:55) Richard: wobei getBangCommand in dem fall mzero ist, wenn das event nicht dem command entspricht
(16:35:59) Richard: könnte man auch anders gestalten
(16:36:17) Richard: oder man bietet registerBangCommand an, was auf registerCallback basiert o.ä.
(16:36:23) Richard: der punkt ist, du kannst jetzt sowas machen:
(16:37:47) Richard: 
bombInit = registerBangCommand "!bomb" $ \args0 -> do
	bomb <- liftIO $ newMVar blub
	pass bomb args0
	registerBangCommand "!pass" $ \args -> pass bomb args
(16:37:53) Richard: o.ä.
(16:38:12) Richard: in dem !pass-callback kannst du jetzt jedenfalls die mvar benutzen, die in !bomb angelegt wurde
(16:38:47) Richard: natürlich müsstest du da noch filtern auf bestimmten channel oder bestimmte user oder je nachdem
(16:39:08) Richard: also so ist es ja so, bei zwei !bomb-befehlen gibt es zwei bomben, die bei einem !pass beide gereicht werden
(16:39:12) Richard: aber details
(16:39:48) Richard: und du solltest auch was anbieten, um die callbacks wieder zu deregistrieren und vllt zur einfacheren benutzung auch registerOnce-funktionen
(16:39:55) Richard: aber so in der art könnte das gehen…
(16:40:18) Richard: aso: und die ganzen callbacks sollten parallel ablaufen
(16:41:16) Richard: und beim start des bots führst du logischerweise die ganzen initialisieren einmal aus und fertsch
-}

runBot :: BotConfig -> IO ()
runBot config = withSocketsDo $ bracket (connect config) disconnect mloop
  where
    disconnect :: BotEnv -> IO ()
    disconnect = hClose . (view socket)
    initState :: BotState
    initState  = BotState [] Nothing [] [] (HandlerId 0)
    mloop :: BotEnv -> IO ()
    mloop env    =  catch (runReaderT (runStateT run initState) env  >> return ())
                        (\(SomeException _) -> return ())

connect :: BotConfig -> IO BotEnv
connect config = notify $ do
        startTime <- getClockTime
        h <- connectTo (view botServer config) (view botPort config)
        hSetBuffering h NoBuffering
        return (BotEnv config h startTime)
    where
        notify = bracket_
            (printf "Connecting to %s ... " (view botServer config) >> hFlush stdout)
            (putStrLn "done.")

run :: Irc ()
run = do
    cfg <- view botConfig
    let nick = view botNick cfg
    let chan = view botChan cfg
    send $ ServerMessage Nothing (Command $ T.pack "NICK") (Param <$> [nick])
    send $ ServerMessage Nothing (Command $ T.pack "USER") (Param <$> [nick, T.pack "0", T.pack "*", T.pack ":MirakelBot"])
    send $ ServerMessage Nothing (Command $ T.pack "JOIN") (Param <$> [chan])
    asks _socket >>= listen

listen :: Handle -> Irc ()
listen h = forever $ do
        rawMessage <- B.init `fmap` liftIO (B.hGetLine h)
        liftIO $ putStrLn $ BC.unpack rawMessage
        let msg = parseOnly parseMessage rawMessage
        either handleError handleSucc msg
    where
        printError = liftIO . putStrLn . show

        handleCmd = maybe (return ()) evalCommand . interpretMessage

        handleError :: String -> Irc ()
        handleError err= do
            lastMessage .= Nothing
            printError err

        handleSucc :: Message -> Irc ()
        handleSucc msg = do
            lastMessage .= Just msg
            handleCmd msg

interpretMessage = undefined
evalCommand = undefined
