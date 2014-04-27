module Bot.Handle where
import Data.List
import Data.Foldable
import Util.Irc
import           Bot.Types
import           Control.Monad.Reader
import           Control.Monad.State
import           System.Exit

updateUserList :: ([User]-> [User]) -> BotState -> BotState
updateUserList f bstate@(BotState {onlineUsers=list})=bstate {onlineUsers = f list}

interpretMessage :: Message -> Maybe Command
interpretMessage (Ping x) = Just $ Pong (':' : x)
interpretMessage (Join user _) = Just $ AddUser user
interpretMessage (UserQuit user) = Just $ DelUser user
interpretMessage (PrivMsg _) =Just $ HandleHotword
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
    traverse_ handleHotword' handlers
    return ()
    where 
        handleHotword' MessageHandler {handlerHotword = hot, handlerHook = func} =func tmsg hot
        handleHotword' _ = return ()

lookupHotword :: String         -- ^ Message
              -> String         -- ^ Prefix-Hotword
              -> [BotHandler]   -- ^ All handler
              -> [BotHandler]   -- ^ Proper handler
lookupHotword msg prefix handlers = 
    case splitAt (length prefix) msg of
        (p,rest) | p==prefix -> filterFirst isPrefix rest handlers
        _ -> filterFirst isInfix msg handlers
    where
        filterFirst f mmsg = filter (f mmsg . handlerHotword)
        isPrefix mmsg (HotwordPrefix hot) = hot `isPrefixOf` mmsg
        isPrefix _ _ = False
        isInfix mmsg (HotwordInfix hot) = hot `isInfixOf` mmsg
        isInfix _ _ = False
