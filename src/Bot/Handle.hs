module Bot.Handle where
import           Bot.NetIO
import Data.List
import Data.Foldable
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
p_text hotword mentionings = (p_hotword hotword)

p_hotword :: String -> CharParser () Command
p_hotword hotword = return HandleHotword

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
interpretMessage (PrivMsg (TextMsg {msgMessage = message})) =Just $ HandleHotword
interpretMessage Other = Nothing

evalCommand :: Command -> Net ()
evalCommand (Pong x) = writeRaw "PONG" [x]
evalCommand (AddUser user) = modify $ updateUserList (user:)
evalCommand (DelUser user) = modify $ updateUserList $ filter (/= user)
evalCommand Quit = writeRaw "QUIT" [":Exiting"] >> liftIO (exitWith ExitSuccess)
evalCommand HandleHotword = gets lastMessage >>= maybe (return ()) handleHotword

handleHotword :: TextMsg -> Net ()
handleHotword tmsg@(TextMsg {msgMessage = msg}) = do
    prefixHotword <- asks $ botHotword . botConfig
    allHandlers <- asks $ botHandlers . botConfig
    let handlers = lookupHotword msg prefixHotword allHandlers
    traverse_ (\(hot, func) -> func tmsg hot) handlers
    return ()

lookupHotword :: String         -- ^ Message
              -> String         -- ^ Prefix-Hotword
              -> [BotHandler]   -- ^ All handler
              -> [BotHandler]   -- ^ Proper handler
lookupHotword msg prefix handlers = 
    case splitAt (length prefix) msg of
        (p,rest) | p==prefix -> filterFirst isPrefix rest handlers
        _ -> filterFirst isInfix msg handlers
    where
        filterFirst f mmsg = filter (f mmsg .fst)
        isPrefix mmsg (HotwordPrefix hot) = hot `isPrefixOf` mmsg
        isPrefix _ _ = False
        isInfix mmsg (HotwordInfix hot) = hot `isInfixOf` mmsg
        isInfix _ _ = False
