{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (parseStringToBf, Token (..), ParseError (..)) where

import Control.Monad (void)
import GHC.Unicode (isSpace)

data Token = Incr | Decr | IncrPtr | DecrPtr | PutChar | GetChar | Loop [Token]
  deriving (Eq, Show)

data ParseError = UnexpectedEof | UnexpectedChar Char | UnknownError
  deriving (Eq, Show)

data ParseResult a = ParseNone | ParseErr ParseError | ParseOk a

newtype Parser a = Parser {runParser :: String -> (ParseResult a, String)}

instance Functor Parser where
  fmap f p = do
    x <- p
    return $ f x

instance Applicative Parser where
  pure x = Parser $ \s -> (ParseOk x, s)
  a <*> b = do
    f <- a
    x <- b
    return $ f x

instance Monad Parser where
  (Parser p) >>= f =
    Parser $ \input ->
      let (result, nextInput) = p input
       in case result of
            ParseNone -> (ParseNone, nextInput)
            ParseErr e -> (ParseErr e, nextInput)
            ParseOk x -> runParser (f x) nextInput

toEither :: ParseError -> ParseResult a -> Either ParseError a
toEither _ (ParseOk x) = Right x
toEither _ (ParseErr x) = Left x
toEither defaultErr ParseNone = Left defaultErr

constParser :: ParseResult a -> Parser a
constParser result = Parser $ \input -> (result, input)

(<|>) :: Parser a -> Parser a -> Parser a
a <|> b = Parser $ \input ->
  case (runParser a input, runParser b input) of
    ((ParseNone, _), x) -> x
    (x, _) -> x

popCharIf :: (Char -> Bool) -> Parser Char
popCharIf cond = Parser pop
  where
    pop (x : xs) | cond x = (ParseOk x, xs)
    pop rest = (ParseNone, rest)

popChar :: Parser Char
popChar = popCharIf (const True)

expect :: Char -> Parser ()
expect c = void (popCharIf (== c))

expectEof :: Parser ()
expectEof = Parser $ \input -> (if null input then ParseOk () else ParseNone, input)

unexpected :: Parser a
unexpected = Parser $ \case
  (x : xs) -> (ParseErr $ UnexpectedChar x, xs)
  [] -> (ParseErr UnexpectedEof, [])

optional :: Parser a -> Parser (Maybe a)
optional parser = Parser $ \input ->
  case runParser parser input of
    (ParseOk x, rest) -> (ParseOk (Just x), rest)
    (ParseNone, rest) -> (ParseOk Nothing, rest)
    (ParseErr e, rest) -> (ParseErr e, rest)

many :: Parser a -> Parser [a]
many parser = do
  result <- optional parser
  case result of
    Nothing -> return []
    Just x -> do
      xs <- many parser
      return (x : xs)

many_ :: Parser a -> Parser ()
many_ parser = (parser >> many_ parser) <|> return ()

skipWhitespaces :: Parser ()
skipWhitespaces = many_ $ popCharIf isSpace

parseSimpleToken :: Parser Token
parseSimpleToken = do
  x <- popChar
  constParser $ case x of
    '+' -> ParseOk Incr
    '-' -> ParseOk Decr
    '>' -> ParseOk IncrPtr
    '<' -> ParseOk DecrPtr
    '.' -> ParseOk PutChar
    ',' -> ParseOk GetChar
    _ -> ParseNone

parseLoopToken :: Parser Token
parseLoopToken = do
  expect '['
  tokens <- many parseToken
  expect ']' <|> unexpected
  return (Loop tokens)

parseToken :: Parser Token
parseToken = skipWhitespaces >> (parseSimpleToken <|> parseLoopToken)

parseBf :: Parser [Token]
parseBf = do
  tokens <- many parseToken <|> unexpected
  skipWhitespaces
  expectEof <|> unexpected
  return tokens

parseStringToBf :: String -> Either ParseError [Token]
parseStringToBf = toEither UnknownError . fst . runParser parseBf
