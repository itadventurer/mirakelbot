module MirakelBot.InitHandlers where
import MirakelBot.Handlers.Id
import MirakelBot.Types

miscHandlers :: [Irc ()]
miscHandlers = [MirakelBot.Handlers.Id.init] 
