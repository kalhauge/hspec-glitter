{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Various git related utils
module Control.Git (
  gitChanges,

  -- * Helpers
  GitStatus (..),
  Status (..),
  parseGitProcelain,
  parseGitProcelainLine,
  parseGitProcelainStatus,
) where

-- text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText

-- typed-process
import System.Process.Typed

-- base

import Control.Applicative
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Char as Char
import Data.Function
import qualified Data.Text as Text
import System.Directory (getCurrentDirectory)
import System.FilePath

{- | Return the difference between the file at the FilePath and the index.
If the file is not in index return Nothing.
-}
gitChanges :: FilePath -> IO [FilePath]
gitChanges filename = do
  (ec, _, _) <-
    proc "git" ["ls-files", "--error-unmatch", filename]
      & readProcess

  gitpath <-
    "git rev-parse --show-toplevel"
      & readProcessStdout_
      >>= \case
        [gitpath] -> pure gitpath
        _ow -> error "Unexpected number of lines in git rev-parse"
        . (fmap LazyText.unpack . LazyText.lines . LazyText.decodeUtf8)

  cd <- getCurrentDirectory

  case ec of
    ExitSuccess -> do
      (items, _) <-
        proc "git" ["status", "--porcelain=v1", filename]
          & readProcess_

      let gits = items & LazyText.decodeUtf8 & parseGitProcelain

      case gits of
        Left msg -> fail msg
        Right res -> do
          pure
            . map (\a -> makeRelative cd (gitpath </> gsFile a))
            . filter (\a -> gsY a `notElem` [Unmodified, Ignored, Deleted])
            $ res
    ExitFailure _ -> do
      proc "git" ["ls-files", "--exclude-standard", "--others", filename]
        & readProcess_
        & fmap (map LazyText.unpack . LazyText.lines . LazyText.decodeUtf8 . fst)

data Status
  = Unmodified
  | Modified
  | TypeChanged
  | Added
  | Deleted
  | Renamed
  | Copied
  | UpdatedButUnmerged
  | Untracked
  | Ignored
  deriving (Show, Enum, Eq, Ord)

data GitStatus = GitStatus
  { gsX :: !Status
  , gsY :: !Status
  , gsFrom :: !(Maybe FilePath)
  , gsFile :: !FilePath
  }
  deriving (Show, Eq)

parseGitProcelain :: LazyText.Text -> Either String [GitStatus]
parseGitProcelain = A.parseOnly (A.manyTill (parseGitProcelainLine A.<?> "single line") A.endOfInput)

parseGitProcelainLine :: A.Parser GitStatus
parseGitProcelainLine = do
  x <- parseGitProcelainStatus A.<?> "x"
  y <- parseGitProcelainStatus A.<?> "y"
  _ <- A.space
  fp1 <- Text.unpack <$> (cStringLiteralParser <|> A.takeWhile1 (not . Char.isSpace))
  res <-
    A.choice
      [ do
          _ <- A.try $ A.string " -> "
          fp2 <- Text.unpack <$> (cStringLiteralParser <|> A.takeWhile1 (not . Char.isSpace))
          pure $ GitStatus{gsX = x, gsY = y, gsFrom = Just fp1, gsFile = fp2}
      , pure $ GitStatus{gsX = x, gsY = y, gsFrom = Nothing, gsFile = fp1}
      ]
  A.endOfLine
  pure res

parseGitProcelainStatus :: A.Parser Status
parseGitProcelainStatus = do
  c <- A.anyChar A.<?> "a status"
  case c of
    ' ' -> pure Unmodified
    'M' -> pure Modified
    'T' -> pure TypeChanged
    'A' -> pure Added
    'D' -> pure Deleted
    'R' -> pure Renamed
    'C' -> pure Copied
    'U' -> pure UpdatedButUnmerged
    '!' -> pure Ignored
    '?' -> pure Untracked
    _ -> fail $ "unexpected " <> show c <> "expected a git status identifier"

-- Parse a C string literal
cStringLiteralParser :: A.Parser Text.Text
cStringLiteralParser =
  A.char '"'
    *> (Text.concat <$> A.many' (stringLiteralContent <|> escapeSequence))
    <* A.char '"'
 where
  stringLiteralContent = A.takeWhile1 (\c -> c /= '"' && c /= '\\')
  escapeSequence =
    Text.singleton <$> do
      _ <- A.char '\\'
      c <- A.anyChar
      case c of
        'n' -> pure '\n'
        't' -> pure '\t'
        '"' -> pure '"'
        '\'' -> pure '\''
        '\\' -> pure '\\'
        _ow -> fail ("Unexpected escape char " <> show c)
