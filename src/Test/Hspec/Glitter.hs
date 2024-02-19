{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Hspec.Glitter (
  -- * Glitter
  Glitter (..),
  glitter,
  glitterWith,
) where

-- directory
import System.Directory

-- filepath
import System.FilePath

-- text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText

-- typed-process
import System.Process.Typed

-- hspec
import Test.Hspec.Core.Spec

-- base
import Data.Foldable
import Data.Function
import Data.IORef

-- | The basic glitter test
data Glitter a = Glitter
  { glitterFile :: !FilePath
  , glitterContent :: !a
  , glitterWriter :: !(FilePath -> a -> IO ())
  , glitterPostcheck :: !Expectation
  , glitterLocation :: !(Maybe Location)
  }

instance Example (Glitter a) where
  type Arg (Glitter a) = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (IO (Glitter a)) where
  type Arg (IO (Glitter a)) = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (arg -> IO (Glitter a)) where
  type Arg (arg -> IO (Glitter a)) = arg
  evaluateExample glitter' _ action _ = do
    ref <- newIORef (Result "" Success)
    action $ \arg -> do
      g <- glitter' arg
      r <- runGlitter g
      writeIORef ref r
    readIORef ref

instance Example (arg -> Glitter a) where
  type Arg (arg -> Glitter a) = arg
  evaluateExample glitter' _ action _ = do
    ref <- newIORef (Result "" Success)
    action $ \arg -> do
      r <- runGlitter (glitter' arg)
      writeIORef ref r
    readIORef ref

glitter :: (HasCallStack) => FilePath -> String -> Glitter String
glitter glitterFile glitterContent =
  Glitter
    { glitterFile = glitterFile
    , glitterContent = glitterContent
    , glitterWriter = writeFile
    , glitterPostcheck = pure ()
    , glitterLocation = location
    }

glitterWith
  :: (HasCallStack)
  => (FilePath -> a -> IO ())
  -> FilePath
  -> a
  -> IO ()
  -> Glitter a
glitterWith glitterWriter glitterFile glitterContent glitterPostcheck =
  Glitter
    { glitterFile = glitterFile
    , glitterContent = glitterContent
    , glitterWriter = glitterWriter
    , glitterPostcheck = glitterPostcheck
    , glitterLocation = location
    }

runGlitter :: Glitter a -> IO Result
runGlitter Glitter{..} = do
  createDirectoryIfMissing True (takeDirectory glitterFile)
  glitterWriter glitterFile glitterContent

  res <- gitDiff glitterFile

  case res of
    Nothing -> safeEvaluate do
      glitterPostcheck
      pure
        . Result ""
        . Failure glitterLocation
        . Reason
        $ fold
          [ "file "
          , show glitterFile
          , " not in index"
          , "\n"
          ]
    Just "" -> do
      --  Assume post-tests have been run.
      pure (Result "" Success)
    Just l -> safeEvaluate do
      glitterPostcheck
      pure
        . Result ""
        . Failure glitterLocation
        . Reason
        $ fold
          [ "file "
          , show glitterFile
          , " did not match index (but passed postchecks)"
          , "\n"
          , LazyText.unpack l
          ]

{- | Return the difference between the file at the FilePath and the index.
If the file is not in index return Nothing.
-}
gitDiff :: FilePath -> IO (Maybe LazyText.Text)
gitDiff filename = do
  ec <-
    proc "git" ["ls-files", "--error-unmatch", filename]
      & setStdout nullStream
      & setStderr nullStream
      & runProcess

  case ec of
    ExitFailure _ ->
      pure Nothing
    ExitSuccess -> do
      (_, diff) <- readProcessStdout (proc "git" ["diff", filename])
      pure . Just $ LazyText.decodeUtf8 diff
