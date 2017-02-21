{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DefaultSignatures #-}
module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Control.Monad.Free as F
import Control.Concurrent.MVar
import Control.Monad (forM, forever)
import Debug.Trace
import Data.Serialize
import GHC.Generics

-- newtype EndpointAddress = EndpointAddress Int

-- class (Show a) => Message a where
--   serialize :: a -> B.ByteString
--   deserialize :: B.ByteString -> a 

-- data AnyMessage = forall a. (Serialize a) => AnyMessage {unMessage :: a}
-- instance Show AnyMessage where
--   show (AnyMessage m) = show m

-- instance Message String where
--   serialize = B.pack
--   deserialize = B.unpack

class SerializableTo a b where
  serialize :: a -> b
  deserialize :: b -> a

instance SerializableTo a a where
  serialize = id
  deserialize = id

instance Serialize a => SerializableTo a B.ByteString where
  serialize = runPut . put
  deserialize = fromRight . runGet get where
    fromRight (Right r) = r
    fromRight (Left l) = error "Either is not Right"

data NetOp address wire next = 
    Send address wire next
  | Receive ((Maybe (address, wire)) -> next)
  | WaitForMessage next
  | GetPeers ([address] -> next)
  | GetSelf (address -> next)
  deriving Functor

type NetM address wire = F.Free (NetOp address wire)

send :: (SerializableTo m w) => a -> m -> NetM a w ()
send addr msg = F.Free $ Send addr (serialize msg) $ F.Pure ()
receive :: (SerializableTo m w) => NetM a w (Maybe (a, m))
receive = F.Free $ Receive $ F.Pure . (fmap (\(a, b) -> (a, deserialize b))) 
getPeers :: forall a . forall w . NetM a w [a]
getPeers = F.Free (GetPeers F.Pure)
getSelf :: forall a . forall w . NetM a w a
getSelf = F.Free $ GetSelf F.Pure

data MessageUnion = SimpleMessage {unSimpleMessage :: String} deriving (Show, Generic)
instance Serialize MessageUnion 
-- instance Message MessageUnion where
--   serialize (SimpleMessage m) = B.pack m
--   deserialize bs = SimpleMessage $ B.unpack bs

program = do
  peers <- getPeers
  self <- getSelf
  forM peers $ \peer -> do
    send peer $ SimpleMessage $ "A" ++ show self
  forever $ do
    receive >>= \m -> case m of
      Just (peer, (SimpleMessage msg)) -> do
        send peer $ SimpleMessage $ "Echo " ++ msg
      otherwise -> return ()

runRepeatly :: a -> (a -> IO a) -> IO ()
runRepeatly a f = do
  r <- f a
  runRepeatly r f

stepNumPerLoop = 10

-- type EmulatorWireFormat = MessageUnion
type EmulatorM w a = NetM Int w a

data EmulatorState w = EmulatorState {messages :: M.Map Int (S.Seq (Int, w))} deriving (Show)

emptyEmulatorState addrs = EmulatorState {
  messages = M.fromList $ zip addrs $ repeat S.empty
}

emulateM :: (Show w) => Int -> EmulatorM w () -> IO ()
emulateM peerNum program = do
  let addrs = [1..peerNum]
  state <- newMVar $ emptyEmulatorState addrs
  runRepeatly (repeat program) $ \continuations -> do
    newContinuations <- emulateRoundM state addrs continuations
    currState <- readMVar state
    putStrLn $ show currState
    return newContinuations

emulateRoundM :: (MVar (EmulatorState w)) -> [Int] -> [EmulatorM w ()] -> IO [EmulatorM w ()]
emulateRoundM state addrs programs = 
  forM (zip addrs programs) $ \(addr, program) ->
    let peers = filter (/= addr) addrs in 
      emulateIndividualMachine state addr peers stepNumPerLoop program

emulateIndividualMachine :: (MVar (EmulatorState w)) -> Int -> [Int] -> Int -> EmulatorM w () -> IO (EmulatorM w ())
emulateIndividualMachine state addr peers stepNum program
  | stepNum == 0 = return program
  | otherwise = case program of
      F.Free (Send destination bs next) -> do
        modifyMVar_ state $ \state -> return $ state {messages = M.adjust (S.|> (addr, bs)) destination $ messages state}
        return next
      F.Free (Receive callback) -> do
        head <- modifyMVar state $ \state -> case M.lookup addr $ messages state of
          Just seq -> case S.viewl seq of
            head S.:< remains -> return $ (state {messages = M.adjust (const remains) addr $ messages state}, Just head)
            S.EmptyL -> return (state, Nothing)
          Nothing -> return (state, Nothing)
        return $ callback head
      F.Free (GetPeers callback) -> do
        return $ callback peers
      F.Free (GetSelf callback) -> do
        return $ callback addr
      _ -> return $ return ()
    

someFunc :: IO ()
someFunc = emulateM 10 (program :: EmulatorM MessageUnion ())
