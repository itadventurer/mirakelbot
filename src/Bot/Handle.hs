module Bot.Handle where
import           Bot.NetIO
import Util.Irc
import           Bot.Types
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           System.Exit
import           Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

updateUserList :: ([User]-> [User]) -> BotState -> BotState
updateUserList f bstate@(BotState {onlineUsers=list})=bstate {onlineUsers = f list}

p_text :: String -> [String] -> CharParser () Command
p_text hotword mentionings = (p_hotword hotword) -- <|> (p_mentioning mentionings)

p_hotword :: String -> CharParser () Command
p_hotword hotword = HandleHotword <$> (string hotword *> (many $ noneOf " ")) <*> (many anyChar)

-- todo
{-
p_mentioning :: [String] -> CharParser () Command
p_mentioning (x:_) = HandleMentioning <$>
    do
        manyTill anyChar (try $ string x)
        many anyChar
-}

interpretMessage :: Message -> Maybe Command
interpretMessage (Ping x) = Just $ Pong (':' : drop 6 x)
interpretMessage (Join user _) = Just $ AddUser user
interpretMessage (UserQuit user) = Just $ DelUser user
interpretMessage (PrivMsg (TextMsg {msgMessage = message})) = either (\_ -> Nothing) Just $ parse (p_text "!" ["mirakel"]) "" message
interpretMessage Other = Nothing

evalCommand :: Command -> Net ()
evalCommand (Pong x) = writeRaw "PONG" [x]
evalCommand (AddUser user) = modify $ updateUserList (user:)
evalCommand (DelUser user) = modify $ updateUserList $ filter (/= user)
evalCommand Quit = writeRaw "QUIT" [":Exiting"] >> liftIO (exitWith ExitSuccess)
evalCommand cmd@(HandleHotword {}) = gets lastMessage >>= maybe (return ()) (handleHotword cmd)
evalCommand (HandleMentioning _ ) = return ()

handleHotword :: Command -> TextMsg -> Net ()
handleHotword HandleHotword {hotHotword = "uptime"} _ = uptime >>= answer
handleHotword HandleHotword {hotHotword = "quit"} TextMsg { msgSender = sender} = do
    masters <- asks $ botMasters . botConfig
    if (userName sender) `elem` masters
    then
        writeRaw "QUIT" [":Exiting"] >> liftIO (exitWith ExitSuccess)
    else
        answer "You are not my master!"
handleHotword HandleHotword {hotHotword = "id", hotParams = msg} _ = answer msg
handleHotword HandleHotword {hotHotword = "users"} _ = do
    users <- gets onlineUsers
    answer $ show $ map userName users
handleHotword _ _ = return ()
