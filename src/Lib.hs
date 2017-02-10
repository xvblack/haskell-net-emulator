{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE DeriveFunctor #-}
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

-- newtype EndpointAddress = EndpointAddress Int

class (Show a) => Message a where
  serialize :: a -> B.ByteString
  deserialize :: B.ByteString -> a 

data AnyMessage = forall a. Message a => AnyMessage {unMessage :: a}
instance Show AnyMessage where
  show (AnyMessage m) = show m

instance Message String where
  serialize = B.pack
  deserialize = B.unpack

data NetOp address next = 
    Send address AnyMessage next
  | Receive ((Maybe (address, B.ByteString)) -> next)
  | WaitForMessage next
  | GetPeers ([address] -> next)
  | GetSelf (address -> next)
  deriving Functor

type NetM address = F.Free (NetOp address)

send :: (Message m) => a -> m -> NetM a ()
send addr msg = F.Free $ Send addr (AnyMessage msg) $ F.Pure ()
receive :: (Message m) => NetM a (Maybe (a, m))
receive = F.Free $ Receive $ F.Pure . (fmap (\(a, b) -> (a, deserialize b))) 
getPeers :: forall a . NetM a [a]
getPeers = F.Free (GetPeers F.Pure)
getSelf :: NetM a a
getSelf = F.Free $ GetSelf F.Pure

program = do
  peers <- getPeers
  self <- getSelf
  forM peers $ \peer -> do
    send peer $ "A" ++ show self
  forever $ do
    receive >>= \m -> case m of
      Nothing -> return ()
      Just (peer, msg) -> do
        traceM msg
        send peer $ "Echo" ++ msg

prettyM :: NetM Int () -> IO ()
prettyM (F.Free (Send addr msg next)) = do
  putStrLn $ "Send" ++ show msg ++ "To" ++ show addr
  prettyM next
prettyM _ = return ()

runRepeatly :: a -> (a -> IO a) -> IO ()
runRepeatly a f = do
  r <- f a
  runRepeatly r f

stepNumPerLoop = 10

data EmulatorState = EmulatorState {messages :: M.Map Int (S.Seq (Int, B.ByteString))} 
  deriving (Show)

emptyEmulatorState addrs = EmulatorState {
  messages = M.fromList $ zip addrs $ repeat S.empty
}

emulateM :: Int -> NetM Int () -> IO ()
emulateM peerNum program = do
  let addrs = [1..peerNum]
  state <- newMVar $ emptyEmulatorState addrs
  runRepeatly (repeat program) $ \continuations -> do
    newContinuations <- emulateRoundM state addrs continuations
    currState <- readMVar state
    putStrLn $ show currState
    return newContinuations

emulateRoundM :: (MVar EmulatorState) -> [Int] -> [NetM Int ()] -> IO [NetM Int ()]
emulateRoundM state addrs programs = 
  forM (zip addrs programs) $ \(addr, program) ->
    let peers = filter (/= addr) addrs in 
      emulateIndividualMachine state addr peers stepNumPerLoop program

emulateIndividualMachine :: (MVar EmulatorState) -> Int -> [Int] -> Int -> NetM Int () -> IO (NetM Int ())
emulateIndividualMachine state addr peers stepNum program
  | stepNum == 0 = return program
  | otherwise = case program of
      F.Free (Send destination (AnyMessage msg) next) -> do
        modifyMVar_ state $ \state -> return $ state {messages = M.adjust (S.|> (addr, serialize $ msg)) destination $ messages state}
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
someFunc = emulateM 10 program
