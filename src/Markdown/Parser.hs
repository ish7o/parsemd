{-# LANGUAGE GADTs, LambdaCase #-}
module Parser where
import Control.Applicative (Alternative (..))
import Control.Monad (void)

data Block
  = CodeBlock (Maybe String) String
  | Heading Int String
  | List [String]
  | Paragraph [Inline]
  deriving Show

data Inline
  = Text String
  | Bold String
  | Italic String
  | Code String
  | Newline
  deriving Show

data MarkdownDoc where
  MarkdownDoc :: [Block] -> MarkdownDoc
  deriving Show

newtype Parser a = Parser {runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \input ->
    case runParser p input of
      Nothing -> Nothing
      Just (x,rest) -> Just (f x,rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)

  pf <*> px = Parser $ \input ->
    case runParser pf input of
      Nothing -> Nothing
      Just (f,rest) -> case runParser px rest of
        Nothing -> Nothing
        Just (x,rest) -> Just (f x, rest)

instance Alternative Parser where
  empty = Parser $ const Nothing

  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      Nothing -> runParser p2 input
      result -> result

instance Monad Parser where
  p >>= f = Parser $ \input ->
    case runParser p input of
      Nothing -> Nothing
      Just (x, rest) -> flip runParser rest $ f x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
    [] -> Nothing
    (x:xs) -> if f x then Just (x,xs) else Nothing

char :: Char -> Parser Char
char c = satisfy (==c)

anyChar :: Parser Char
anyChar = Parser $ \case
  [] -> Nothing
  (x:xs) -> Just (x,xs)

string :: String -> Parser String
string [] = pure []
string (c:cs) =
  char c >>
  string cs >>= \rest ->
  return (c:rest)

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = (end >> pure []) <|> (:) <$> p <*> manyTill p end

eof :: Parser ()
eof = Parser $ \case
    [] -> Just ((),[])
    _ -> Nothing

skipWhitespace :: Parser ()
skipWhitespace = void $ many (char '\n' <|> char ' ' <|> char '\t')

-- Inlines

parseBold = do
  string "**"
  text <- manyTill anyChar (char '*')
  char '*'
  pure $ Bold text

parseItalic = do
  char '*'
  text <- manyTill anyChar (char '*')
  pure $ Italic text

parseCode = do
  char '`'
  text <- manyTill anyChar (char '`')
  pure $ Code text

parseText = do
  text <- some (satisfy (`notElem` ['*', '`', '\n']))
  pure $ Text text

parseStrikethrough = undefined

parseNewline = do
  char '\n'
  pure Newline

parseInline = parseBold <|> parseItalic <|> parseCode <|> parseText <|> parseNewline

parseInlines = many parseInline

-- Blocks

parseHeading :: Parser Block
parseHeading = do
  hashes <- some (char '#')
  char ' '
  text <- some (satisfy (/= '\n'))
  void (char '\n') <|> eof
  pure $ flip Heading text $ length hashes

parseCodeBlock :: Parser Block
parseCodeBlock = do
  string "```"
  lang <- many (satisfy (/='\n'))
  char '\n'
  code <- manyTill anyChar (string "```")
  case lang of
    [] -> pure $ CodeBlock Nothing code
    _ ->  pure $ CodeBlock (Just lang) code

parseList :: Parser Block
parseList = do
  l <- parseListItem
  rest <- many parseListItem
  pure $ List $ l:rest
  where
    parseListItem = do
      char '-'
      -- <|> char '*'
      char ' '
      manyTill anyChar (char '\n')

parseParagraph :: Parser Block
parseParagraph = Paragraph . concat <$> manyTill parseInlines (void (string "\n\n") <|> eof)

parseBlock :: Parser Block
parseBlock = (
  parseHeading
  <|> parseCodeBlock
  <|> parseList
  <|> parseParagraph
  ) <* (skipWhitespace <|> eof)

parseBlocks :: Parser [Block]
parseBlocks = many parseBlock

parseDoc :: String -> MarkdownDoc
parseDoc input = case runParser parseBlocks input of
  Nothing -> error "failed to parse"
  Just (doc, _) -> MarkdownDoc doc
