{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Hspec.Glitter (
  -- * Glitter
  onGlitter,
  onGlitterEach,
  onGlitterWith,
  onGlitterEachWith,
) where

-- hspec

import Test.Hspec.Core.Hooks
import Test.Hspec.Core.Spec

-- hspec-glitter
import Control.Git
import Control.Monad (zipWithM_)
import Data.Function

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

onGlitterEachWith
  :: (HasCallStack)
  => FilePath
  -- ^ The file or folder to monitor
  -> (a -> FilePath -> IO ())
  -- ^ The command to populate the folder or file with data.
  -> SpecWith FilePath
  -- ^ The tests to run on the changed files.
  -> SpecWith a
onGlitterEachWith fp runner spec = do
  describe fp do
    beforeAllWith (\a -> runner a fp >> gitChanges fp) do
      _ <-
        spec & mapSpecItem_ \item ->
          item
            { itemExample = \pr han pc ->
                pc & itemExample item pr \ac ->
                  han \cs ->
                    let n = length cs
                     in zipWithM_ (\i c -> pc (i, n) >> ac c) [0 ..] cs
            }

      it "should not have changed" $ \changes -> do
        Result "" $ case changes of
          [] -> Success
          xs -> Failure location (Reason $ unlines $ "The following files have changed:" : xs)

onGlitterEach
  :: (HasCallStack)
  => FilePath
  -- ^ The file or folder to monitor
  -> (FilePath -> IO ())
  -- ^ The command to populate the folder or file with data.
  -> SpecWith FilePath
  -- ^ The tests to run on the changed files.
  -> SpecWith a
onGlitterEach fp runner = onGlitterEachWith fp (const runner)
