module MirakelBot where
import MirakelBot.Types
import MirakelBot.Net
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network
import           System.IO
import Control.Lens
import qualified Data.Text as T
import Control.Applicative
import          Options.Applicative
import Data.Monoid
import MirakelBot.InitHandlers
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
    mloop env    =  catch (runReaderT (runStateT (run miscHandlers) initState) env  >> return ())
                        (\(SomeException _) -> return ())

main :: IO (BotConfig)
main = do
    execParser opts
    where
        opts = info (helper <*> parser) mempty
        parser = BotConfig
                    <$> strOption
                        ( short 's'
                       <> long "server"
                       <> metavar "SERVER"
                       <> help "IRC Server where I should connect to" )
                    <*> (PortNumber . fromIntegral <$> option
                        ( short 'p'
                       <> long "port"
                       <> metavar "PORT"
                       <> value (6667 :: Int) -- To avoid "Defaulting the following constraint(s) to type ‘Integer’"
                       <> help "Port" ))
                    <*> (T.pack <$> strOption
                        ( short 'c'
                       <> long "chan"
                       <> metavar "CHANNEL"
                       <> help "Channel" ))
                    <*> (T.pack <$> strOption
                        ( short 'r'
                       <> long "real"
                       <> metavar "REAL"
                       <> help "Real name" ))
                    <*> (T.pack <$> strOption
                        ( short 'n'
                       <> long "nick"
                       <> metavar "NICK"
                       <> help "Nick name" ))
                    <*> (T.pack <$> strOption
                        ( long "hotword"
                       <> metavar "HOTWORD"
                       <> help "The prefix hotword" ))
                    {-
                    <*> many (strOption
                        ( short 'm'
                       <> long "masters"
                       <> metavar "MASTERS"
                       <> help "List of master users" ))
                    <*> pure []-}
