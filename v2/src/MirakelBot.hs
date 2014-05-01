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
