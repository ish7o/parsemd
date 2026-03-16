module Main where

import Markdown.Parser (parseDoc)

main :: IO ()
main = do
  input <- getContents
  print input

  putStrLn "\n\nOut:"
  print $ parseDoc input


