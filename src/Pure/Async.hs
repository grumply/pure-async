{-# LANGUAGE MagicHash, ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications, RecordWildCards #-}
module Pure.Async (async, asyncOnce, asyncOnceAs) where

import Pure.Data.Default
import Pure.Data.View
import Pure.Data.View.Patterns

import Control.Concurrent
import Control.Monad
import Data.Kind
import Data.Typeable
import GHC.Exts

data Async a = Async
  { action :: (IO ())
  , view :: View
  , once :: Bool
  }

async_ :: Typeable a => Async a -> View
async_ = LibraryComponentIO $ \self ->
  def
    { construct = do
        Async {..} <- ask self
        forkIO action
    , receive = \Async { action = new, .. } tid -> do
        Async { action = old } <- ask self
        if not once && not (isTrue# (reallyUnsafePtrEquality# new old)) then do
          killThread tid
          forkIO new
        else do
          return tid
    , render = \Async {..} _ -> view
    , unmounted = do
        tid <- get self
        killThread tid
    }

-- | Execute the given action asynchronously before the given View is first
-- rendered or when the action changes.
--
-- Use as:
--
-- > async someIOAction someView
--
async :: IO () -> View -> View
async action view = async_ (Async action view False :: Async ())


-- | Execute the given action asynchronously before the given View is first
-- rendered.
--
-- Use as:
--
-- > asyncOnce someIOAction someView
--
asyncOnce :: IO () -> View -> View
asyncOnce = asyncOnceAs @()

-- | Execute the given action asynchronously before the given View is first
-- rendered. Requires a visible type application for some hairy situations
-- that require special-case diffing of async once views.
--
-- Use as:
--
-- > asyncOnceAs @SomeType someIOAction someView
--
asyncOnceAs :: forall a. Typeable a => IO () -> View -> View
asyncOnceAs action view = async_ (Async action view True :: Async a)
