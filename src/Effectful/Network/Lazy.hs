{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Wrapper of 'Network.Socket.Lazy.ByteString' for the 'effectful' ecosystem.
-- Please see the documentation of 'Network.Socket.Lazy.ByteString' on how to use
-- this library.
module Effectful.Network.Lazy where

import Effectful (Eff, type (:>))

import Effectful.Dispatch.Static (unsafeEff_)

import Data.ByteString.Lazy (LazyByteString)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy qualified as S
import Prelude hiding (getContents)

import Data.Int (Int64)
import Effectful.Network (Network)

#if MIN_VERSION_network(3,2,0)
import System.Posix.Types (Fd)
#endif

-- | Wraps 'S.send'.
send :: (Network :> es) => Socket -> LazyByteString -> Eff es Int64
send sock = unsafeEff_ . S.send sock

-- | Wraps 'S.sendAll'.
sendAll :: (Network :> es) => Socket -> LazyByteString -> Eff es ()
sendAll sock = unsafeEff_ . S.sendAll sock

#if MIN_VERSION_network(3,2,0)
-- | Wraps 'S.sendWithFds'.
sendWithFds :: (Network :> es) => Socket -> LazyByteString -> [Fd] -> Eff es ()
sendWithFds sock lbs = unsafeEff_ . S.sendWithFds sock lbs
#endif

-- | Wraps 'S.getContents'.
getContents :: (Network :> es) => Socket -> Eff es LazyByteString
getContents = unsafeEff_ . S.getContents

-- | Wraps 'S.recv'.
recv :: (Network :> es) => Socket -> Int64 -> Eff es LazyByteString
recv sock = unsafeEff_ . S.recv sock
