{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Network
  ( Network
  , runNetwork
  , accept
  , bind
  , close
  , close'
  , connect
  , fdSocket
  , getAddrInfo
  , getCloseOnExec
  , getNameInfo
  , getNonBlock
  , getPeerCredential
  , getPeerName
  , getSockOpt
  , getSocketName
  , getSocketOption
  , getSocketType
  , gracefulClose
  , ifIndexToName
  , ifNameToIndex
  , listen
  , mkSocket
  , openSocket
  , recvBuf
  , recvBufFrom
  , recvBufMsg
  , recvFd
  , sendBuf
  , sendBufMsg
  , sendBufTo
  , sendFd
  , setCloseOnExecIfNeeded
  , setNonBlockIfNeeded
  , setSockOpt
  , setSocketOption
  , shutdown
  , socket
  , socketPair
  , socketPort
  , socketPortSafe
  , socketToFd
  , socketToHandle
  , touchSocket
  , unsafeFdSocket
  , whenSupported
  , withFdSocket
  , withSocketsDo
  , module Data.Word
  , module Foreign.C.Types
  , module Foreign.Ptr
  , module Foreign.Storable
  , module GHC.IO.Handle.Types
  , module Network.Socket
  , module System.IO
  ) where

import Effectful
  ( Dispatch (Static)
  , DispatchOf
  , Eff
  , Effect
  , IOE
  , type (:>)
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects)
  , StaticRep
  , evalStaticRep
  , unsafeEff_
  , unsafeLiftMapIO
  , unsafeSeqUnliftIO
  )

import Data.Word (Word8)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import GHC.IO.Handle.Types (Handle)
import Network.Socket
  ( AddrInfo (..)
  , Cmsg (..)
  , Family (..)
  , HostName (..)
  , MsgFlag (..)
  , NameInfoFlag (..)
  , PortNumber (..)
  , ProtocolNumber (..)
  , ServiceName (..)
  , ShutdownCmd (..)
  , SockAddr (..)
  , Socket (..)
  , SocketOption (..)
  , SocketType (..)
  )
import Network.Socket qualified as S
import System.IO (IOMode)

data Network :: Effect

type instance DispatchOf Network = 'Static 'WithSideEffects
newtype instance StaticRep Network = Unit ()

-- | Run the 'Socket' effect.
runNetwork :: (IOE :> es) => Eff (Network : es) a -> Eff es a
runNetwork = evalStaticRep (Unit ())

-- | Wraps 'S.getAddrInfo'.
getAddrInfo
  :: (Network :> es)
  => Maybe AddrInfo
  -> Maybe HostName
  -> Maybe ServiceName
  -> Eff es [AddrInfo]
getAddrInfo addrInfo hostName serviceName = unsafeEff_ $ S.getAddrInfo addrInfo hostName serviceName
{-# INLINE getAddrInfo #-}

-- | Wraps 'S.withSocketsDo'.
withSocketsDo :: (Network :> es) => Eff es a -> Eff es a
withSocketsDo = unsafeLiftMapIO S.withSocketsDo
{-# INLINE withSocketsDo #-}

-- | Wraps 'S.connect'.
connect :: (Network :> es) => Socket -> SockAddr -> Eff es ()
connect sock sockAddr = unsafeEff_ $ S.connect sock sockAddr
{-# INLINE connect #-}

-- | Wraps 'S.bind'.
bind :: (Network :> es) => Socket -> SockAddr -> Eff es ()
bind sock sockAddr = unsafeEff_ $ S.bind sock sockAddr
{-# INLINE bind #-}

-- | Wraps 'S.listen'.
listen :: (Network :> es) => Socket -> Int -> Eff es ()
listen sock n = unsafeEff_ $ S.listen sock n
{-# INLINE listen #-}

-- | Wraps 'S.accept'.
accept :: (Network :> es) => Socket -> Eff es (Socket, SockAddr)
accept sock = unsafeEff_ $ S.accept sock
{-# INLINE accept #-}

-- | Wraps 'S.close'.
close :: (Network :> es) => Socket -> Eff es ()
close = unsafeEff_ . S.close
{-# INLINE close #-}

-- | Wraps 'S.close'.
close' :: (Network :> es) => Socket -> Eff es ()
close' = unsafeEff_ . S.close
{-# INLINE close' #-}

-- | Wraps 'S.gracefulClose'.
gracefulClose :: (Network :> es) => Socket -> Int -> Eff es ()
gracefulClose sock = unsafeEff_ . S.gracefulClose sock
{-# INLINE gracefulClose #-}

-- | Wraps 'S.shutdown'.
shutdown
  :: (Network :> es) => Socket -> ShutdownCmd -> Eff es ()
shutdown sock = unsafeEff_ . S.shutdown sock
{-# INLINE shutdown #-}

-- | Wraps 'S.whenSupported'.
whenSupported
  :: (Network :> es) => SocketOption -> Eff es a -> Eff es ()
whenSupported = unsafeLiftMapIO . S.whenSupported
{-# INLINE whenSupported #-}

-- | Wraps 'S.getSocketOption'.
getSocketOption
  :: (Network :> es) => Socket -> SocketOption -> Eff es Int
getSocketOption sock = unsafeEff_ . S.getSocketOption sock
{-# INLINE getSocketOption #-}

-- | Wraps 'S.setSocketOption'.
setSocketOption
  :: (Network :> es) => Socket -> SocketOption -> Int -> Eff es ()
setSocketOption sock sockOpt = unsafeEff_ . S.setSocketOption sock sockOpt
{-# INLINE setSocketOption #-}

-- | Wraps 'S.getSockOpt'.
getSockOpt
  :: (Network :> es)
  => forall a
   . (Storable a)
  => Socket
  -> SocketOption
  -> Eff es a
getSockOpt sock = unsafeEff_ . S.getSockOpt sock
{-# INLINE getSockOpt #-}

-- | Wraps 'S.setSockOpt'.
setSockOpt
  :: (Network :> es) => (Storable a) => Socket -> SocketOption -> a -> Eff es ()
setSockOpt sock sockOpt = unsafeEff_ . S.setSockOpt sock sockOpt
{-# INLINE setSockOpt #-}

-- | Wraps 'S.socket'.
socket
  :: (Network :> es) => Family -> SocketType -> ProtocolNumber -> Eff es Socket
socket fam sockType = unsafeEff_ . S.socket fam sockType
{-# INLINE socket #-}

-- | Wraps 'S.openSocket'.
openSocket :: (Network :> es) => AddrInfo -> Eff es Socket
openSocket = unsafeEff_ . S.openSocket
{-# INLINE openSocket #-}

-- | Wraps 'S.withFdSocket'.
withFdSocket
  :: (Network :> es) => Socket -> (CInt -> Eff es r) -> Eff es r
withFdSocket sock cb = unsafeSeqUnliftIO $ \unlift -> do
  S.withFdSocket sock (unlift . cb)
{-# INLINE withFdSocket #-}

-- | Wraps 'S.unsafeFdSocket'.
unsafeFdSocket :: (Network :> es) => Socket -> Eff es CInt
unsafeFdSocket = unsafeEff_ . S.unsafeFdSocket
{-# INLINE unsafeFdSocket #-}

-- | Wraps 'S.touchSocket'.
touchSocket :: (Network :> es) => Socket -> Eff es ()
touchSocket = unsafeEff_ . S.touchSocket
{-# INLINE touchSocket #-}

-- | Wraps 'S.socketToFd'.
socketToFd :: (Network :> es) => Socket -> Eff es CInt
socketToFd = unsafeEff_ . S.socketToFd
{-# INLINE socketToFd #-}

-- | Wraps 'S.fdSocket'.
fdSocket :: (Network :> es) => Socket -> Eff es CInt
fdSocket = unsafeEff_ . S.fdSocket
{-# INLINE fdSocket #-}
{-# DEPRECATED fdSocket "Use withFdSocket or unsafeFdSocket instead" #-}

-- | Wraps 'S.mkSocket'.
mkSocket :: (Network :> es) => CInt -> Eff es Socket
mkSocket = unsafeEff_ . S.mkSocket
{-# INLINE mkSocket #-}

-- | Wraps 'S.socketToHandle'.
socketToHandle
  :: (Network :> es) => Socket -> IOMode -> Eff es Handle
socketToHandle sock = unsafeEff_ . S.socketToHandle sock
{-# INLINE socketToHandle #-}

-- | Wraps 'S.getSocketType'.
getSocketType :: (Network :> es) => Socket -> Eff es SocketType
getSocketType = unsafeEff_ . S.getSocketType
{-# INLINE getSocketType #-}

-- | Wraps 'S.getPeerName'.
getPeerName :: (Network :> es) => Socket -> Eff es SockAddr
getPeerName = unsafeEff_ . S.getPeerName
{-# INLINE getPeerName #-}

-- | Wraps 'S.getSocketName'.
getSocketName :: (Network :> es) => Socket -> Eff es SockAddr
getSocketName = unsafeEff_ . S.getSocketName
{-# INLINE getSocketName #-}

-- | Wraps 'S.ifNameToIndex'.
ifNameToIndex :: (Network :> es) => String -> Eff es (Maybe Int)
ifNameToIndex = unsafeEff_ . S.ifNameToIndex
{-# INLINE ifNameToIndex #-}

-- | Wraps 'S.ifIndexToName'.
ifIndexToName :: (Network :> es) => Int -> Eff es (Maybe String)
ifIndexToName = unsafeEff_ . S.ifIndexToName
{-# INLINE ifIndexToName #-}

-- | Wraps 'S.socketPortSafe'.
socketPortSafe
  :: (Network :> es) => Socket -> Eff es (Maybe PortNumber)
socketPortSafe = unsafeEff_ . S.socketPortSafe
{-# INLINE socketPortSafe #-}

-- | Wraps 'S.socketPort'.
socketPort :: (Network :> es) => Socket -> Eff es PortNumber
socketPort = unsafeEff_ . S.socketPort
{-# INLINE socketPort #-}

-- | Wraps 'S.socketPair'.
socketPair
  :: (Network :> es)
  => Family
  -> SocketType
  -> ProtocolNumber
  -> Eff es (Socket, Socket)
socketPair fam sock = unsafeEff_ . S.socketPair fam sock
{-# INLINE socketPair #-}

-- | Wraps 'S.sendFd'.
sendFd :: (Network :> es) => Socket -> CInt -> Eff es ()
sendFd sock = unsafeEff_ . S.sendFd sock
{-# INLINE sendFd #-}

-- | Wraps 'S.recvFd'.
recvFd :: (Network :> es) => Socket -> Eff es CInt
recvFd = unsafeEff_ . S.recvFd
{-# INLINE recvFd #-}

-- | Wraps 'S.getPeerCredential'.
getPeerCredential
  :: (Network :> es) => Socket -> Eff es (Maybe CUInt, Maybe CUInt, Maybe CUInt)
getPeerCredential = unsafeEff_ . S.getPeerCredential
{-# INLINE getPeerCredential #-}

-- | Wraps 'S.getNameInfo'.
getNameInfo
  :: (Network :> es)
  => [NameInfoFlag]
  -> Bool
  -> Bool
  -> SockAddr
  -> Eff es (Maybe HostName, Maybe ServiceName)
getNameInfo flag b1 b2 = unsafeEff_ . S.getNameInfo flag b1 b2
{-# INLINE getNameInfo #-}

-- | Wraps 'S.setCloseOnExecIfNeeded'.
setCloseOnExecIfNeeded :: (Network :> es) => CInt -> Eff es ()
setCloseOnExecIfNeeded = unsafeEff_ . S.setCloseOnExecIfNeeded
{-# INLINE setCloseOnExecIfNeeded #-}

-- | Wraps 'S.getCloseOnExec'.
getCloseOnExec :: (Network :> es) => CInt -> Eff es Bool
getCloseOnExec = unsafeEff_ . S.getCloseOnExec
{-# INLINE getCloseOnExec #-}

-- | Wraps 'S.setNonBlockIfNeeded'.
setNonBlockIfNeeded :: (Network :> es) => CInt -> Eff es ()
setNonBlockIfNeeded = unsafeEff_ . S.setNonBlockIfNeeded
{-# INLINE setNonBlockIfNeeded #-}

-- | Wraps 'S.getNonBlock'.
getNonBlock :: (Network :> es) => CInt -> Eff es Bool
getNonBlock = unsafeEff_ . S.getNonBlock
{-# INLINE getNonBlock #-}

-- | Wraps 'S.sendBuf'.
sendBuf
  :: (Network :> es) => Socket -> Ptr Word8 -> Int -> Eff es Int
sendBuf sock ptr = unsafeEff_ . S.sendBuf sock ptr
{-# INLINE sendBuf #-}

-- | Wraps 'S.recvBuf'.
recvBuf
  :: (Network :> es) => Socket -> Ptr Word8 -> Int -> Eff es Int
recvBuf sock ptr = unsafeEff_ . S.recvBuf sock ptr
{-# INLINE recvBuf #-}

-- | Wraps 'S.sendBufTo'.
sendBufTo
  :: (Network :> es) => Socket -> Ptr a -> Int -> SockAddr -> Eff es Int
sendBufTo sock ptr n = unsafeEff_ . S.sendBufTo sock ptr n
{-# INLINE sendBufTo #-}

-- | Wraps 'S.recvBufFrom'.
recvBufFrom
  :: (Network :> es) => Socket -> Ptr a -> Int -> Eff es (Int, SockAddr)
recvBufFrom sock ptr = unsafeEff_ . S.recvBufFrom sock ptr
{-# INLINE recvBufFrom #-}

-- | Wraps 'S.sendBufMsg'.
sendBufMsg
  :: (Network :> es)
  => Socket
  -> SockAddr
  -> [(Ptr Word8, Int)]
  -> [Cmsg]
  -> MsgFlag
  -> Eff es Int
sendBufMsg sock sockAddr ptrs cmsgs = unsafeEff_ . S.sendBufMsg sock sockAddr ptrs cmsgs
{-# INLINE sendBufMsg #-}

-- | Wraps 'S.recvBufMsg'.
recvBufMsg
  :: (Network :> es)
  => Socket
  -> [(Ptr Word8, Int)]
  -> Int
  -> MsgFlag
  -> Eff es (SockAddr, Int, [Cmsg], MsgFlag)
recvBufMsg sock ptrs n = unsafeEff_ . S.recvBufMsg sock ptrs n
{-# INLINE recvBufMsg #-}
