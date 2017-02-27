{-# LANGUAGE FlexibleContexts #-}
module Visualizer where

import qualified Data.ByteString.Char8 as B
import Prelude hiding (init)
import Reflex.Dom as D
import Emulator
import Control.Monad
import qualified Data.Map as M
import Data.Time.Clock
import Echo (echo)
import Program
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Tuple
import Wire

widget = getCurrentTime >>= \curr ->
    mainWidget $ do
        c <- checkbox False def
        time <- tickLossy 0.5 curr
        let timePausible = attachWithMaybe combine (current $ value c) time
        stream <- stateStream program timePausible
        dyn =<< mapDyn visualizeState stream
        return ()
        where
            combine a b = case a of
                True -> Just b
                False -> Nothing

endpointNum = 3

program :: (Show w, SerializableTo MessageUnion w) => EmulatorProgram [IntAddressNetProgram w ()] w
program = fromIdenticalProgram endpointNum echo

stateStream :: (Show w, SerializableTo MessageUnion w, Reflex t, MonadHold t m, MonadFix m) => EmulatorProgram [IntAddressNetProgram w ()] w -> Event t a -> m (Dynamic t (EmulatorState w))
stateStream program tickStream = mapDyn fst =<< foldDyn f initState tickStream where
    initState = (emptyEmulatorState $ (addrs program :: [Int]), init program)
    f tick (state, cont) = swap $ (step program) state cont

visualizeState :: (MonadWidget t m) => EmulatorState MessageUnion -> m ()
visualizeState (EmulatorState {messages = messages}) = el "div" $ do
    forM_ (M.toList messages) $ \(a, m) -> do
        el "div" $ do
            el "h2" $ text $ show a
            el "div" $ text $ show m

-- visualizeStateAsMessageUnion :: (MonadWidget t m) => EmulatorState MessageUnion -> m ()
-- visualizeStateAsMessageUnion = visualizeState