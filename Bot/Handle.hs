module Bot.Handle where
import           Bot.Data

updateUserList :: BotState -> ([User]-> [User]) -> BotState
updateUserList state@(BotState {onlineUsers=list}) f=state {onlineUsers = f list}

interpretMessage :: Message -> Command
interpretMessage (Ping x) = Pong (':' : drop 6 x)
interpretMessage (Join user _) = UpdateUser (\xs -> user:xs)
interpretMessage (UserQuit user) = UpdateUser (\xs -> filter (\x -> x/=user) xs)
interpretMessage msg@(PrivMsg {}) = undefined
interpretMessage Other = DoNothing

evalCommand :: Command -> Net ()
evalCommand _ = undefined
