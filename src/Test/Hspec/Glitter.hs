{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Hspec.Glitter (
  -- * Golden
  golden,
  goldenWith,

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

-- base
import Control.Monad (zipWithM_)
import Data.Function

-- | A simple golden test. Fails if the content of the FilePath have changed.
golden
  :: (HasCallStack)
  => FilePath
  -> (FilePath -> IO ())
  -> Spec
golden fp fn = onGlitter fp fn (pure ())

{- | A simple golden test (given an input argument).
Fails if the content of the FilePath have changed.
-}
goldenWith
  :: (HasCallStack)
  => FilePath
  -> (a -> FilePath -> IO ())
  -> SpecWith a
goldenWith fp fn = onGlitterWith fp fn (pure ())

{- | Given a filepath, a function to populate that folder, and a
specification tree with a list of changed filepaths.
-}
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

-- | Like @onGlitterWith@ but maps over the changed files one by one.
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
  onGlitterWith fp runner $
    spec & mapSpecItem_ \item ->
      item
        { itemExample = \pr handler pc ->
            pc & itemExample item pr \ac ->
              handler \cs ->
                let n = length cs
                 in zipWithM_ (\i c -> pc (i, n) >> ac c) [0 ..] cs
        }

-- | onGlitterEachWith but without a spec argument.
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
