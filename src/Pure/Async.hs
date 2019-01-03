{-# LANGUAGE MagicHash #-}
module Pure.Async where

import Pure.Data.Default
import Pure.Data.View
import Pure.Data.View.Patterns

import Control.Concurrent
import Control.Monad

import GHC.Exts

newtype Async a = Async (a,View)

async :: IO () -> View -> View
async x v = flip LibraryComponentIO (Async (x,v)) $ \self ->
  def
    { construct = return ()
    , executing = do
        Async (x,_) <- ask self
        void $ forkIO x
    , receive = \(Async (x,v)) _ -> do
        Async (x',_) <- ask self
        when (isTrue# (reallyUnsafePtrEquality# x x')) x
        return ()
    , render = \(Async (_,v)) _ -> v
    }
