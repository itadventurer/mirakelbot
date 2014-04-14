module Bot.Handle where
import           Bot.Data
import           Bot.NetIO

updateUserList :: BotState -> ([User]-> [User]) -> BotState
updateUserList state@(BotState {onlineUsers=list}) f=state {onlineUsers = f list}

{-isHot :: String -> String -> Bool
isHot command text = (hotword ++ command) `isPrefixOf` text
eval :: Message -> Net ()
eval msg@(Message {messageText = text, messageAuthor = author})
    -- Hotwords
    | isHot "quit" text     = if author `elem` masters
                            then write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
                            else pubmsg "You are not my master!"
    | isHot "uptime" text   = uptime >>= pubmsg
    | isHot "id" text       = pubmsg (drop (2+ (length hotword)) text)
    | isHot "users" text    = do users <- gets onlineUsers
                                 pubmsg $ show users
    -- Mention Mirakel
    | "irakel" `isInfixOf` text  = handleMentioning msg
    -- Meta stuff
eval _                        = return () -- ignore everything else

handleMentioning :: Message -> Net ()
handleMentioning Message {messageText = text, messageAuthor = author, messageIsPrivate = isPrivate } = do
    privmsg author $ unwords ["Hey",author, "you've mentioned Mirakel!"]
    privmsg author "Can I help you?"
-}
interpretMessage :: Message -> Command
interpretMessage (Ping x) = Pong (':' : drop 6 x)
interpretMessage (Join user _) = AddUser user
interpretMessage (UserQuit user) = DelUser user
interpretMessage msg@(PrivMsg {}) = undefined
interpretMessage Other = DoNothing

evalCommand :: Command -> Net ()
evalCommand (Pong x) = write "PONG" x
