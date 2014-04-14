module Bot.Handle where
import           Bot.Data
import           Bot.NetIO
import           Control.Monad.State
import           Control.Monad.Reader
import           System.Exit
import           Control.Applicative
import           Control.Monad
import           Data.Functor
import           Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

updateUserList :: ([User]-> [User]) -> BotState -> BotState
updateUserList f bstate@(BotState {onlineUsers=list})=bstate {onlineUsers = f list}

{-isHot :: String -> String -> Bool
isHot command text = (hotword ++ command) `isPrefixOf` text
eval :: Message -> Net ()
eval msg@(Message {messageText = text, messageAuthor = author})
    -- Hotwords
    | isHot "users" text    = do users <- gets onlineUsers
                                 pubmsg $ show users
handleMentioning :: Message -> Net ()
handleMentioning Message {messageText = text, messageAuthor = author, messageIsPrivate = isPrivate } = do
    privmsg author $ unwords ["Hey",author, "you've mentioned Mirakel!"]
    privmsg author "Can I help you?"
-}

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
