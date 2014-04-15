module Bot.Parser (parseMessage) where
import           Bot.Types
import           Control.Applicative
import           Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

parseMessage :: CharParser () Message
parseMessage =  try p_ping
            <|> try p_other
            <|> return Other

p_ping :: CharParser () Message
p_ping = Ping <$> (string "PING :" *> many anyChar)


p_other :: CharParser () Message
p_other =   try p_join
        <|> try p_quit
        <|> try p_privMsg

p_join, p_quit, p_privMsg :: CharParser () Message
p_join = Join <$> (p_user <* (p_cmd "JOIN")) <*> p_channels
p_quit = UserQuit <$> (p_user <* (p_cmd "QUIT"))
p_privMsg = PrivMsg <$> p_textMsg

p_textMsg :: CharParser () TextMsg
p_textMsg = TextMsg <$> (p_user <* p_cmd "PRIVMSG") <*> p_name <*> p_message

p_user :: CharParser () User
p_user = do
    _ <- char ':'
    User <$> (many $ noneOf " !") <*> (char '!' *> (many $ noneOf " "))

p_cmd :: String -> CharParser () String
p_cmd cmd = between spaces spaces (string cmd)

p_channels :: CharParser () [Channel]
p_channels = (many $ noneOf " ") `sepBy` (char ' ')

p_name :: CharParser () String
p_name = many $ noneOf " "

p_message :: CharParser () String
p_message = do
    spaces
    _ <- char ':'
    many anyChar
