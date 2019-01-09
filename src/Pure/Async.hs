{-# LANGUAGE MagicHash, ScopedTypeVariables, AllowAmbiguousTypes, PolyKinds, TypeInType #-}
module Pure.Async (Async(..),async,asyncAs) where

import Pure.Data.Default
import Pure.Data.View
import Pure.Data.View.Patterns

import Control.Concurrent
import Control.Monad
import Data.Kind
import Data.Typeable
import GHC.Exts

data Async (a :: k) = Async (IO ()) View

-- | Execute the given action asynchronously when the given View is rendered.
-- Use as:
--
-- > async (Async someIO someView :: Async someTypeOrKind)
--
async :: forall k a. (Typeable (a :: k), Typeable k) => Async a -> View
async = LibraryComponentIO $ \self ->
  def
    { construct = return ()
    , executing = do
        Async x _ <- ask self
        void $ forkIO x
    , receive = \(Async new v) _ -> do
        Async old _ <- ask self
        unless (isTrue# (reallyUnsafePtrEquality# new old)) new
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
-- For a poly-kinded version, use asyncAs'
asyncAs :: forall a. (Typeable (a :: *)) => IO () -> View -> View
asyncAs io v = async (Async io v :: Async a)

-- | A version of `async` that requires TypeApplications rather than an explicit
-- data constructor + type signature.
--
-- This version of `async` allows for poly-kinded Async types.
--
-- Instead of:
--
-- > async (Async someIO someView :: Async somePolyKind)
--
-- Use this:
--
-- > asyncAs @_ @somePolyKind someIO someView
--
asyncAs' :: forall k a. (Typeable (a :: k), Typeable k) => IO () -> View -> View
asyncAs' io v = async (Async io v :: Async a)

