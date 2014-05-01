module MirakelBot.InitHandlers where
import           MirakelBot.Handlers.Id
import           MirakelBot.Handlers.ServerComm
import           MirakelBot.Types

miscHandlers :: [Irc ()]
miscHandlers = [MirakelBot.Handlers.Id.init,MirakelBot.Handlers.ServerComm.init]
