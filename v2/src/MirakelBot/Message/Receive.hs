module MirakelBot.Message.Receive where
import MirakelBot.Types
import Control.Applicative ((<$>),(<*>), (<|>), some,many)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as AC
import Text.Read

isSpace  :: Char -> Bool
isSpace ' '     = True
isSpace '\n'    = True
isSpace '\r'    = True
isSpace _       = False

decodeMessage :: ByteString -> Maybe Message
decodeMessage = undefined

parseMessage :: A.Parser Message
parseMessage = do
    prefix  <- A.option Nothing $ Just <$> parsePrefix
    command <- parseCommand
    params  <- A.manyTill parseParam A.endOfInput
    return $ ServerMessage prefix command params

-- |<prefix>   ::= ':' (<servername> | <nick> [ '!' <user> ] [ '@' <host> ])
parsePrefix :: A.Parser Prefix
parsePrefix = do
    _ <- AC.char8 ':'
    prefix <- AC.takeTill $ \x -> isSpace x || x== '!' || x =='@'
    if '.' `BC.elem` prefix
        then return $ ServerPrefix $ T.decodeUtf8 prefix
        else do
            user <- A.option Nothing $ Just <$> do
                _ <- AC.char8 '!'
                AC.takeTill $ (||) <$> isSpace <*> (=='@')

            host <- A.option Nothing $ Just <$> do
                _ <- AC.char8 '@'
                AC.takeTill isSpace

            return $ NickPrefix (Nick $ T.decodeUtf8 prefix)
                                (User <$> T.decodeUtf8 <$> user)
                                (Host <$> T.decodeUtf8 <$> host)

parseCommand :: A.Parser Command
parseCommand = do
    -- remove whitespace
    AC.skipWhile isSpace
    parseNormalCommand <|> parseNumericCommand
    where
        parseNormalCommand = do
            cmd <- some $ AC.satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'])
            return $ Command $ T.pack cmd
        parseNumericCommand = do
            cmd <- AC.take 3
            case readMaybe $ BC.unpack cmd of
                Nothing -> fail "Invalid command"
                Just a -> return $ NumericCommand a

parseParams = A.manyTill parseParam A.endOfInput

parseParam :: A.Parser Param
parseParam = do
    AC.skipWhile isSpace
    Param <$> T.decodeUtf8 <$> (parseTrailing <|> parseMiddle)
    where
        parseTrailing = do
            _ <- AC.char8 ':'
            AC.takeTill $ (||) <$> (== '\r') <*> (== '\n')
        parseMiddle = AC.takeTill isSpace
