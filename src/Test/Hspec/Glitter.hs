{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Hspec.Glitter (
  -- * Glitter
  onGlitterWith,
) where

-- text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText

-- typed-process
import System.Process.Typed

-- hspec
import Test.Hspec.Core.Spec

-- base

import Control.Applicative
import qualified Data.Attoparsec.Text.Lazy as A
import Data.Function
import qualified Data.Text as Text
import Test.Hspec.Core.Hooks

-- | Give a folder or file, run a command
onGlitterWith
  :: (HasCallStack)
  => FilePath
  -- ^ The file or folder to monitor
  -> (FilePath -> a -> IO ())
  -- ^ The command to populate the folder or file with data.
  -> SpecWith [FilePath]
  -- ^ The tests to run on the changed files.
  -> SpecWith a
onGlitterWith fp runner spec = do
  describe fp do
    beforeAllWith (\a -> runner fp a >> gitChanges fp) do
      _ <- spec
      it "should not have changed" $ \changes -> do
        Result "" $ case changes of
          [] -> Success
          xs -> Failure location (Reason $ unlines $ "The following files have changed:" : xs)

{- | Return the difference between the file at the FilePath and the index.
If the file is not in index return Nothing.
-}
gitChanges :: FilePath -> IO [FilePath]
gitChanges filename = do
  (_, items) <-
    proc "git" ["status", "--porcelain=v1", filename]
      & readProcessStdout

  items
    & LazyText.decodeUtf8
    & parseGitProcelain
    & either
      fail
      (pure . map gsFile . filter (\a -> a.gsY /= Unmodified && a.gsX /= Ignored))

data Status
  = Unmodified
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
parseGitProcelain =
  A.parseOnly . many $ do
    x <- parseStatus
    y <- parseStatus
    _ <- A.space
    fp1 <- Text.unpack <$> A.takeWhile1 (/= ' ')
    res <-
      asum
        [ do
            _ <- A.string " -> "
            fp2 <- Text.unpack <$> A.takeWhile1 (/= ' ')
            pure $ GitStatus{gsX = x, gsY = y, gsFrom = Just fp1, gsFile = fp2}
        , pure $ GitStatus{gsX = x, gsY = y, gsFrom = Nothing, gsFile = fp1}
        ]
    A.endOfLine
    pure res
 where
  parseStatus = do
    c <- A.anyChar
    case c of
      ' ' -> pure Unmodified
      'T' -> pure TypeChanged
      'A' -> pure Added
      'D' -> pure Deleted
      'R' -> pure Renamed
      'C' -> pure Copied
      'U' -> pure UpdatedButUnmerged
      '!' -> pure Ignored
      '?' -> pure Untracked
      _ -> fail $ "unexpected " <> show c <> "expected a git status identifier"

-- TODO Fix

-- handler cmd (t, b) = do
--   case t of
--     Just "" -> throwIO (Pending loc (Just "File have not changed"))
--     _ow -> cmd b

-- onGlitterWith :: (HasCallStack) => FilePath -> (FilePath -> a -> IO ()) -> SpecWith FilePath -> SpecWith a
-- onGlitterWith fp runner test = do
--   describe fp do
--     beforeAllWith
--       ( \a -> do
--           runner fp a
--           t <- gitDiff fp
--           pure (t, fp)
--       )
--       do
--         aroundWith handler test
--         it "should not have changed" \(t, _) -> do
--           diffToResult fp loc t
--  where
--   loc = location
--   handler cmd (t, b) = do
--     case t of
--       Just "" -> throwIO (Pending loc (Just "File have not changed"))
--       _ow -> cmd b
--
-- -- | The basic glitter test
-- data Glitter a = Glitter
--   { glitterFile :: !FilePath
--   , glitterContent :: !a
--   , glitterWriter :: !(FilePath -> a -> IO ())
--   , glitterPostcheck :: !Expectation
--   , glitterLocation :: !(Maybe Location)
--   }
--
-- instance Example (Glitter a) where
--   type Arg (Glitter a) = ()
--   evaluateExample e = evaluateExample (\() -> e)
--
-- instance Example (IO (Glitter a)) where
--   type Arg (IO (Glitter a)) = ()
--   evaluateExample e = evaluateExample (\() -> e)
--
-- instance Example (arg -> IO (Glitter a)) where
--   type Arg (arg -> IO (Glitter a)) = arg
--   evaluateExample glitter' _ action _ = do
--     ref <- newIORef (Result "" Success)
--     action $ \arg -> do
--       g <- glitter' arg
--       r <- runGlitter g
--       writeIORef ref r
--     readIORef ref
--
-- instance Example (arg -> Glitter a) where
--   type Arg (arg -> Glitter a) = arg
--   evaluateExample glitter' _ action _ = do
--     ref <- newIORef (Result "" Success)
--     action $ \arg -> do
--       r <- runGlitter (glitter' arg)
--       writeIORef ref r
--     readIORef ref
--
-- glitter :: (HasCallStack) => FilePath -> String -> Glitter String
-- glitter glitterFile glitterContent =
--   Glitter
--     { glitterFile = glitterFile
--     , glitterContent = glitterContent
--     , glitterWriter = writeFile
--     , glitterPostcheck = pure ()
--     , glitterLocation = location
--     }
--
-- glitterWith
--   :: (HasCallStack)
--   => (FilePath -> a -> IO ())
--   -> FilePath
--   -> a
--   -> IO ()
--   -> Glitter a
-- glitterWith glitterWriter glitterFile glitterContent glitterPostcheck =
--   Glitter
--     { glitterFile = glitterFile
--     , glitterContent = glitterContent
--     , glitterWriter = glitterWriter
--     , glitterPostcheck = glitterPostcheck
--     , glitterLocation = location
--     }
--
-- runGlitter :: Glitter a -> IO Result
-- runGlitter Glitter{..} = do
--   createDirectoryIfMissing True (takeDirectory glitterFile)
--   glitterWriter glitterFile glitterContent
--
--   res <- gitDiff glitterFile
--
--   case res of
--     Nothing -> safeEvaluate do
--       glitterPostcheck
--       pure
--         . Result ""
--         . Failure glitterLocation
--         . Reason
--         $ fold
--           [ "file "
--           , show glitterFile
--           , " not in index"
--           , "\n"
--           ]
--     Just "" -> do
--       --  Assume post-tests have been run.
--       pure (Result "" Success)
--     Just l -> safeEvaluate do
--       glitterPostcheck
--       pure
--         . Result ""
--         . Failure glitterLocation
--         . Reason
--         $ fold
--           [ "file "
--           , show glitterFile
--           , " did not match index (but passed postchecks)"
--           , "\n"
--           , LazyText.unpack l
--           ]

-- diffToResult :: (Show a) => a -> Maybe Location -> Maybe LazyText.Text -> Result
-- diffToResult file loc = \case
--   Nothing ->
--     Result ""
--       . Failure loc
--       . Reason
--       $ fold ["file ", show file, " not in index", "\n"]
--   Just "" ->
--     Result "" Success
--   Just l ->
--     Result ""
--       . Failure loc
--       . Reason
--       $ fold ["file ", show file, " did not match index (but passed postchecks)", "\n", LazyText.unpack l]

-- {- | Return the difference between the file at the FilePath and the index.
-- If the file is not in index return Nothing.
-- -}
-- gitDiff :: FilePath -> IO (Maybe LazyText.Text)
-- gitDiff filename = do
--   ec <-
--     proc "git" ["ls-files", "--error-unmatch", filename]
--       & setStdout nullStream
--       & setStderr nullStream
--       & runProcess
--
--   case ec of
--     ExitFailure _ ->
--       pure Nothing
--     ExitSuccess -> do
--       (_, diff) <- readProcessStdout (proc "git" ["diff", filename])
--       pure . Just $ LazyText.decodeUtf8 diff
