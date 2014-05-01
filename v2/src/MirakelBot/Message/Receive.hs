{-# LANGUAGE OverloadedStrings #-}
module MirakelBot.Message.Receive where
import           Control.Applicative   (some, (<$>), (<*>), (<|>))
import qualified Data.Attoparsec       as A
import qualified Data.Attoparsec.Char8 as AC
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           MirakelBot.Types
import           Text.Read

isSpace  :: Char -> Bool
isSpace ' '     = True
isSpace '\n'    = True
isSpace '\r'    = True
isSpace _       = False

decodeMessage :: ByteString -> Maybe Message
decodeMessage rawMessage =
        let msg =AC.parseOnly parseMessage rawMessage in
        either (const Nothing) Just msg

{-
<command>  ::= <letter> { <letter> } | <number> <number> <number>
<SPACE>    ::= ' ' { ' ' }
<params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]

<middle>   ::= <Any *non-empty* sequence of octets not including SPACE
               or NUL or CR or LF, the first of which may not be ':'>
<trailing> ::= <Any, possibly *empty*, sequence of octets not including
                 NUL or CR or LF>

<crlf>     ::= CR LF

----

   <channel>    ::= ('#' | '&') <chstring>
   <servername> ::= <host>
   <host>       ::= see RFC 952 [DNS:4] for details on allowed hostnames
   <nick>       ::= <letter> { <letter> | <number> | <special> }
   <mask>       ::= ('#' | '$') <chstring>
   <chstring>   ::= <any 8bit code except SPACE, BELL, NUL, CR, LF and
                     comma (',')>

   Other parameter syntaxes are:

   <user>       ::= <nonwhite> { <nonwhite> }
   <letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
   <number>     ::= '0' ... '9'
   <special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'


 -}
parseMessage :: A.Parser Message
parseMessage = do
    prefix  <- A.option Nothing $ Just <$> parsePrefix
    command <- parseCommand
    params  <- parseParams
    return $ toMsg prefix command params
    where
        toMsg :: Maybe Prefix -> Command -> [Param] -> Message
        toMsg (Just prefix@(NickPrefix {})) PRIVMSG (to:params) = PrivateMessage (Just $ getSender prefix) (getTo to) $ T.unwords $ map getParam params
        toMsg p c px= ServerMessage p c px

        getSender :: Prefix -> To
        getSender (NickPrefix  {prefixNick = nick}) = ToNick nick
        getSender _ = error "This could not happen"

        getTo :: Param -> [To]
        getTo (Param to) = map pTo $ T.split (== ',') to

        pTo to
            | "#" `T.isPrefixOf` to =ToChannel $ Channel to
            | T.any (=='@') to = ToUser $ User to
            | otherwise = ToNick $ Nick to

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
            let mcmd = readMaybe cmd :: Maybe Command
            return $ fromMaybe (Command $ T.pack cmd) mcmd
        parseNumericCommand = do
            cmd <- AC.take 3
            case readMaybe $ BC.unpack cmd of
                Nothing -> fail "Invalid command"
                Just a -> return $ NumericCommand a

parseParams :: A.Parser [Param]
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
