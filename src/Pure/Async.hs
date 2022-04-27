{-# language PatternSynonyms, BlockArguments, LambdaCase, ScopedTypeVariables, FlexibleContexts #-}
module Pure.Async where

import Pure.Elm.Fold (Elm,View,Time,loop,continue,fold,foldM,it,command,delay,pattern Microseconds,pattern Null)

import Control.Concurrent (forkIO,killThread,newEmptyMVar,tryPutMVar,takeMVar,putMVar)
import Control.Exception (SomeException,evaluate,handle,throw)
import Control.Monad (void,unless)
import Data.Function (fix)
import Data.Typeable (Typeable)
import System.Timeout (timeout)
import System.IO.Unsafe (unsafeDupablePerformIO,unsafePerformIO)

{-
NOTES:

The goal of this library is to help enrich simple functional `View`-based code 
with a class of IO effects that can be safely performed outside of the normal
RealWorld context. Idempotent requests for data backed by a caching mechanism, 
like HTTP GET, for example, are a good use case.

The combination of purified IO effects when paired with a means of introspecting
on the duration of their evaluation allows for simplified coding styles in some 
critical cases.

The pair of `suspense` and `eval` are powerful, and dangerous. Be careful that you
understand what could happen with their use, like duplicated evaluation of 
`eval` computations. Limited uses can be made to be safe in this model, but 
care must be taken. See, for example, pure-magician, where certain types of data
requests are purified from IO.

-}

{-# NOINLINE eval #-}
-- | Extremely unsafe. `eval` is just a synonym for `unsafePerformIO`. 
eval :: IO b -> b
eval = unsafePerformIO 

async :: View -> View
async x = foldM (\x _ -> pure (x :: Maybe View)) (command (Just x) >> pure (Nothing,pure ())) (maybe Null id it)

-- | `suspense` can help to make certain `eval`-based code safer by imposing a 
-- thread barrier on a set of asyncronous computations. The intention is to 
-- generically handle slow-to-build `View`s, especially views using `eval` 
-- computation purification.  
--
-- > class Get a b | b -> a where
-- >   get :: a -> b 
-- > instance Get (Slug Post) Post where
-- >   get = eval (req ...)
-- > instance Get Username User where
-- >   get = eval (req ...)
-- > instance Get (Slug Post) Comments where
-- >   get = eval (req ...)
-- >
-- > post slug = 
-- >   suspense (Milliseconds 500 0) spinner $
-- >     let 
-- >       Post { author, title, content } = get slug
-- >     in 
-- >       Article <||>
-- >         [ let User { realName } = get author
-- >           in Header <||> 
-- >                 [ H1 <||> title
-- >                 , P <||> [ "By ", txt realName ]
-- >                 ]
-- >
-- >       , Section <||> content
-- >
-- >       , suspense (Milliseconds 500 0) spinner $
-- >           let Comments { comments } = delayed Second (get slug)
-- >           in Footer <||> comments
-- >
-- >       ]
--
-- Note that eval calls are localized to the view and thus the outer suspense
-- will be for the Post request and the User request, but the nested Comments 
-- request will be localized to it's own suspense wrapper. But, the Comments
-- suspense will not trigger until after both the Post and User responses have
-- materialized. 
--
suspense :: Time -> View -> View -> View
suspense t sus = suspense' [(t,sus)] Null

suspense' :: [(Time,View)] -> View -> View -> View
suspense' tvs failure v_ = 
  foldM (\new _ -> pure new) start it
  where
    start :: Elm View => IO (View,IO ())
    start = do
      mv <- newEmptyMVar

      t1 <- forkIO do
        loop tvs \case
          [] -> do
            x <- takeMVar mv
            command x
          (Microseconds us _,v):tvs -> do
            mx <- timeout us (takeMVar mv)
            case mx of
              Nothing -> command v >> continue tvs
              Just x  -> command x

      t2 <- forkIO do
        void do
          handle (\(se :: SomeException) -> print se >> tryPutMVar mv failure) do
            v <- evaluate v_ 
            tryPutMVar mv v

      pure (Null,killThread t1 >> killThread t2)

-- | Delay the materialization of a value. This can be useful for creating 
-- suspense where it might not always exist. Some UIs need to be slowed 
-- down to improve the user experience, and careful use of `delayed` is a 
-- simple primitive that can serve that purpose.
--
-- > post slug = o
-- >   suspense (Milliseconds 500 0) spinner do
-- >     delayed Second do
-- >       let
-- >         Post { author } = get slug
-- >         Author { realName, bio } = get author
-- >       {...}
-- > 
delayed :: Time -> a -> a
delayed t a = 
  unsafeDupablePerformIO do
    delay t
    pure a
