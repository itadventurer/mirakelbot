module Main where
import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.Text               as T
import           MirakelBot.InitHandlers
import           MirakelBot.Net
import           MirakelBot.Types
import           Network
import           Options.Applicative
import           System.IO

runBot :: BotConfig -> IO ()
runBot config = withSocketsDo $ bracket (connect config) disconnect mloop
  where
    disconnect :: BotEnv -> IO ()
    disconnect = hClose . view socket
    mloop :: BotEnv -> IO ()
    mloop env    =  catch (void (runReaderT (run miscHandlers) env))
                        handleException
    handleException :: SomeException -> IO ()
    handleException e = putStrLn $ show e

main :: IO ()
main = execParser opts >>= runBot
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
