module MirakelBot.InitHandlers where
import MirakelBot.Handlers.Id
import MirakelBot.Types
import MirakelBot.Handlers.ServerComm

miscHandlers :: [Irc ()]
miscHandlers = [MirakelBot.Handlers.Id.init,MirakelBot.Handlers.ServerComm.init] 
