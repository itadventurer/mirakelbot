module Bot.Handle where
import           Bot.NetIO
import           Bot.Types
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor
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
interpretMessage msg@(PrivMsg {msgMessage = message}) = either (\_ -> Nothing) Just $ parse (p_text "!" ["mirakel"]) "" message
interpretMessage Other = Nothing

evalCommand :: Command -> Net ()
evalCommand (Pong x) = write "PONG" x
evalCommand (AddUser user) = modify $ updateUserList (user:)
evalCommand (DelUser user) = modify $ updateUserList $ filter (/= user)
evalCommand Quit = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
evalCommand cmd@(HandleHotword {}) = gets lastMessage >>= maybe (return ()) (handleHotword cmd)
evalCommand (HandleMentioning _ ) = return ()

handleHotword :: Command -> Message -> Net ()
handleHotword HandleHotword {hotHotword = "uptime"} _ = uptime >>= pubmsg
handleHotword HandleHotword {hotHotword = "quit"} PrivMsg { msgSender = sender} = do
    masters <- asks $ botMasters . botConfig
    if (userName sender) `elem` masters
    then
        write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
    else
        pubmsg "You are not my master!"
handleHotword HandleHotword {hotHotword = "id", hotParams = msg} _ = pubmsg msg
handleHotword HandleHotword {hotHotword = "users"} _ = do
    users <- gets onlineUsers
    pubmsg $ show $ map userName users
handleHotword _ _ = return ()
