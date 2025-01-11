{-# LANGUAGE OverloadedStrings #-}

module Three where

import Control.Applicative (Alternative (many))
import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    choice,
    decimal,
    parseOnly,
  )
import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text.IO as TIO

data Term
  = Mul Int Int
  | Junk Char
  | DoTerm
  | DontTerm
  deriving (Eq, Show)

solve :: IO ()
solve =
  do
    text <- TIO.readFile "./puzzle-inputs/three/input.txt"
    let program = successfulParse text
    print $ part1 program
    print $ part2 program

part1, part2 :: [Term] -> Int
part1 = sum . fmap evalTerm
part2 = sum . fmap evalTerm . enabledMuls

evalTerm :: Term -> Int
evalTerm (Mul a b) = a * b
evalTerm _ = 0

enabledMuls :: [Term] -> [Term]
enabledMuls terms = snd $ foldl' go (True, []) terms
  where
    go (True, ts) (Mul a b) = (True, Mul a b : ts)
    go (_, ts) DontTerm = (False, ts)
    go (_, ts) DoTerm = (True, ts)
    go (c, ts) _ = (c, ts)

junkP :: Parser Term
junkP = Junk <$> anyChar

doP :: Parser Term
doP = DoTerm <$ "do()"

dontP :: Parser Term
dontP = DontTerm <$ "don't()"

mulP :: Parser Term
mulP = Mul <$> ("mul(" *> decimal <* ",") <*> decimal <* ")"

termsP :: Parser [Term]
termsP = many (choice [mulP, doP, dontP, junkP])

successfulParse :: Text -> [Term]
successfulParse input =
  case parseOnly termsP input of
    Left _err -> []
    Right matches -> matches
