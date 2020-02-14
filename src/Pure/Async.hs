{-# LANGUAGE MagicHash, ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications, RecordWildCards #-}
module Pure.Async (async, asyncAs, asyncOnce, asyncOnceAs) where

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
async_ = Component $ \self ->
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
-- Note: If the supplied action is updated, the thread running the original 
-- action will be killed. Be sure to guard the action as necessary fro your 
-- use-case.
--
-- Note: The asynchronous action will be killed via `killThread` when the
-- view is unmounted.
async :: IO () -> View -> View
async action view = async_ (Async action view False :: Async ())

-- | Execute the given action asynchronously before the given View is first
-- rendered. Requires a visible type application for some hairy situations
-- that require special-case diffing of async views.
--
-- Use as:
--
-- > asyncAs @SomeType someIOAction someView
--
-- Note: If the supplied action is updated, the thread running the original 
-- action will be killed. Be sure to guard the action as necessary fro your 
-- use-case.
--
-- Note: The asynchronous action will be killed via `killThread` when the
-- view is unmounted.
asyncAs :: forall a. Typeable a => IO () -> View -> View
asyncAs action view = async_ (Async action view False :: Async a)

-- | Execute the given action asynchronously before the given View is first
-- rendered.
--
-- Use as:
--
-- > asyncOnce someIOAction someView
--
-- Note: If the supplied action is updated, the thread running the original 
-- action will be killed. Be sure to guard the action as necessary fro your 
-- use-case.
--
-- Note: The asynchronous action will be killed via `killThread` when the
-- view is unmounted.
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
-- Note: The asynchronous action will be killed via `killThread` when the
-- view is unmounted.
asyncOnceAs :: forall a. Typeable a => IO () -> View -> View
asyncOnceAs action view = async_ (Async action view True :: Async a)
