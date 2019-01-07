{-# LANGUAGE MagicHash, ScopedTypeVariables, AllowAmbiguousTypes #-}
module Pure.Async (Async(..),async,asyncAs) where

import Pure.Data.Default
import Pure.Data.View
import Pure.Data.View.Patterns

import Control.Concurrent
import Control.Monad
import Data.Typeable

import GHC.Exts

data Async a = Async (IO ()) View

-- | Execute the given action asynchronously when the given View is rendered.
-- Use as:
--
-- > async (Async someIO someView :: Async someType)
--
async :: Typeable a => Async a -> View
async = LibraryComponentIO $ \self ->
  def
    { construct = return ()
    , executing = do
        Async x _ <- ask self
        void $ forkIO x
    , receive = \(Async x v) _ -> do
        Async x' _ <- ask self
        when (isTrue# (reallyUnsafePtrEquality# x x')) x
        return ()
    , render = \(Async _ v) _ -> v
    }


-- | A version of `async` that requires TypeApplications rather than an explicit
-- data constructor + type signature.
--
-- Instead of:
--
-- > async (Async someIO someView :: Async someType)
--
-- Use this:
--
-- > asyncAs @someType someIO someView
--
asyncAs :: forall a. Typeable a => IO () -> View -> View
asyncAs io v = async (Async io v :: Async a)
