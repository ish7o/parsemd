{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Char (isDigit)

data ListType = Ordered | Unordered
  deriving (Show)

data ListItem = ListItem [Inline] [ListItem]
  deriving (Show)

data Block
  = CodeBlock (Maybe String) String
  | Heading Int String
  | List ListType [ListItem]
  | Paragraph [Inline]
  deriving (Show)

data Inline
  = Text String
  | Bold String
  | Italic String
  | Code String
  | Link [Inline] String
  | Newline
  deriving (Show)

data MarkdownDoc where
  MarkdownDoc :: [Block] -> MarkdownDoc
  deriving (Show)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f p = Parser $ \input ->
    case runParser p input of
      Nothing -> Nothing
      Just (x, rest) -> Just (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)

  pf <*> px = Parser $ \input ->
    case runParser pf input of
      Nothing -> Nothing
      Just (f, rest) -> case runParser px rest of
        Nothing -> Nothing
        Just (x, rest) -> Just (f x, rest)

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
  (x : xs) -> if f x then Just (x, xs) else Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar :: Parser Char
anyChar = Parser $ \case
  [] -> Nothing
  (x : xs) -> Just (x, xs)

string :: String -> Parser String
string [] = pure []
string (c : cs) =
  char c
    >> string cs
    >>= \rest ->
      return (c : rest)

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = (end >> pure []) <|> (:) <$> p <*> manyTill p end

someTill :: Parser a -> Parser end -> Parser [a]
someTill p end = do
  f <- p
  rest <- manyTill p end
  pure $ f : rest

eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
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

parseLink = do
  char '['
  text <- someTill anyChar (char ']')
  let Just (inlines, _) = runParser parseInlines text
  char '('
  link <- someTill anyChar (char ')')
  pure $ Link inlines link

-- char '('
-- link <- someTill anyChar (char ')')
-- pure $ Link text link
--
parseText = do
  text <- some (satisfy (`notElem` ['*', '`', '\n']))
  pure $ Text text

parseStrikethrough = undefined

parseNewline = do
  char '\n'
  pure Newline

parseInline =
  parseBold
    <|> parseItalic
    <|> parseCode
    <|> parseLink
    <|> parseText
    <|> parseNewline

parseInlines :: Parser [Inline]
parseInlines = some parseInline

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
  lang <- many (satisfy (/= '\n'))
  char '\n'
  code <- manyTill anyChar (string "```")
  case lang of
    [] -> pure $ CodeBlock Nothing code
    _ -> pure $ CodeBlock (Just lang) code

parseUnorderedList :: Int -> Parser Block
parseUnorderedList level = List Unordered <$> some (parseListItem level)
  where
    parseListItem level = do
      indent <- sum . map (\x -> if x == ' ' then 1 else 2) <$> many (char ' ' <|> char '\t')
      if indent == level
        then do
          string "- "
          text <- manyTill anyChar (void (char '\n') <|> eof)
          case runParser parseInlines text of
            Nothing -> empty
            Just (inlines, _) -> do
              ListItem inlines <$> many (parseListItem $ level + 2)
        else empty

parseOrderedList :: Int -> Parser Block
parseOrderedList level = List Ordered <$> some (parseListItem level)
  where
    parseListItem level = do
      indent <- sum . map (\x -> if x == ' ' then 1 else 2) <$> many (char ' ' <|> char '\t')
      if indent == level
        then do
          satisfy isDigit
          string ". "
          text <- manyTill anyChar $ void (char '\n') <|> eof
          case runParser parseInlines text of
            Nothing -> empty
            Just (inlines, _) -> ListItem inlines <$> many (parseListItem $ level + 2)
        else empty

parseParagraph :: Parser Block
parseParagraph = Paragraph <$> someTill parseInline (void (string "\n\n") <|> eof)

parseBlock :: Parser Block
parseBlock =
  ( parseHeading
      <|> parseCodeBlock
      <|> parseUnorderedList 0
      <|> parseOrderedList 0
      <|> parseParagraph
  )
    <* (skipWhitespace <|> eof)

parseBlocks :: Parser [Block]
parseBlocks = some parseBlock

parseDoc :: String -> MarkdownDoc
parseDoc input = case runParser parseBlocks input of
  Nothing -> error "failed to parse"
  Just (doc, _) -> MarkdownDoc doc
