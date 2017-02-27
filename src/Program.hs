{-# LANGUAGE DeriveFunctor, DeriveGeneric, DefaultSignatures, AllowAmbiguousTypes, RankNTypes, FlexibleContexts #-}
module Program where

import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Control.Monad.Free as F
-- import Control.Concurrent.TMVar
import Control.Monad (forM, forever)
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Debug.Trace
import Data.Serialize
import Data.Tuple
import GHC.Generics
import Control.Concurrent.MonadIO
import Control.Concurrent.STM.MonadIO hiding (modifyTMVar)
import Wire
import Network
import Emulator
import Prelude hiding (init)

data NetOp address wire next = 
    Send address wire next
  | Receive ((Maybe (address, wire)) -> next)
  | WaitForMessage next
  | GetPeers ([address] -> next)
  | GetSelf (address -> next)
  deriving Functor

type NetProgram address wire = F.Free (NetOp address wire)

send :: (SerializableTo m w) => a -> m -> NetProgram a w ()
send addr msg = F.Free $ Send addr (serialize msg) $ F.Pure ()
receive :: (SerializableTo m w) => NetProgram a w (Maybe (a, m))
receive = F.Free $ Receive $ F.Pure . (fmap (\(a, b) -> (a, deserialize b))) 
getPeers :: forall a . forall w . NetProgram a w [a]
getPeers = F.Free (GetPeers F.Pure)
getSelf :: forall a . forall w . NetProgram a w a
getSelf = F.Free $ GetSelf F.Pure

stepNumPerLoop = 10

-- type EmulatorWireFormat = MessageUnion
type IntAddressNetProgram w a = NetProgram Int w a

emulateRoundM :: (Show w, NetworkM m Int, MonadState (EmulatorState w) m) => [Int] -> [IntAddressNetProgram w ()] -> m [IntAddressNetProgram w ()]
emulateRoundM addrs programs = 
  forM (zip addrs programs) $ \(addr, program) ->
    let peers = filter (/= addr) addrs in 
      emulateIndividualMachine addr peers program

updateState :: (MonadState s m) => (s -> s) -> m ()
updateState f = do
    state $ \x -> ((), f x)

emulateIndividualMachine :: (Show w, NetworkM m Int, MonadState (EmulatorState w) m) => Int -> [Int] -> IntAddressNetProgram w () -> m (IntAddressNetProgram w ())
emulateIndividualMachine addr peers program = case program of
      F.Free (Send destination bs next) -> do
        updateState $ \state -> state {messages = M.adjust (S.|> (addr, bs)) destination $ messages state}
        return next
      F.Free (Receive callback) -> do
        head <- state $ \state -> swap $ case M.lookup addr $ messages state of
          Just seq -> case S.viewl seq of
            head S.:< remains -> (state {messages = M.adjust (const remains) addr $ messages state}, Just head)
            S.EmptyL -> (state, Nothing)
          Nothing -> (state, Nothing)
        return $ callback head
      F.Free (GetPeers callback) -> do
        return $ callback peers
      F.Free (GetSelf callback) -> do
        return $ callback addr
      _ -> return $ return ()

fromIdenticalProgram :: (Show w) => Int -> IntAddressNetProgram w () -> EmulatorProgram [IntAddressNetProgram w ()] w
fromIdenticalProgram peerNum program =
  let addrs = [1..peerNum]
      state = emptyEmulatorState addrs in
  EmulatorProgram {
      addrs = addrs,
      init = take peerNum $ repeat program,
      step = \state continuations -> (flip runState) state $ emulateRoundM addrs continuations
  }