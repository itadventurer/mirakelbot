module MirakelBot.InitHandlers where
import           MirakelBot.Handlers.Id
import           MirakelBot.Handlers.ServerComm
import           MirakelBot.Handlers.Talk
import           MirakelBot.Handlers.Mirakel
import           MirakelBot.Handlers.Users
import           MirakelBot.Handlers.Fun
import           MirakelBot.Handlers.Help
import           MirakelBot.Types

miscHandlers :: [Irc ()]
miscHandlers = [MirakelBot.Handlers.Id.init,MirakelBot.Handlers.ServerComm.init,MirakelBot.Handlers.Talk.init,MirakelBot.Handlers.Mirakel.init,MirakelBot.Handlers.Users.init,MirakelBot.Handlers.Help.init,MirakelBot.Handlers.Fun.init]
