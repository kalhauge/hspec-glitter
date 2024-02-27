{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Hspec.Glitter (
  -- * Glitter
  onGlitter,
  onGlitterWith,
) where

-- hspec

import Test.Hspec.Core.Hooks
import Test.Hspec.Core.Spec

-- hspec-glitter
import Control.Git

-- | Given a folder or file, run a command
onGlitterWith
  :: (HasCallStack)
  => FilePath
  -- ^ The file or folder to monitor
  -> (a -> FilePath -> IO ())
  -- ^ The command to populate the folder or file with data.
  -> SpecWith [FilePath]
  -- ^ The tests to run on the changed files.
  -> SpecWith a
onGlitterWith fp runner spec = do
  describe fp do
    beforeAllWith (\a -> runner a fp >> gitChanges fp) do
      _ <- spec
      it "should not have changed" $ \changes -> do
        Result "" $ case changes of
          [] -> Success
          xs -> Failure location (Reason $ unlines $ "The following files have changed:" : xs)

-- | Given a folder or file, run a command
onGlitter
  :: (HasCallStack)
  => FilePath
  -- ^ The file or folder to monitor
  -> (FilePath -> IO ())
  -- ^ The command to populate the folder or file with data.
  -> SpecWith [FilePath]
  -- ^ The tests to run on the changed files.
  -> SpecWith a
onGlitter fp runner = onGlitterWith fp (const runner)
