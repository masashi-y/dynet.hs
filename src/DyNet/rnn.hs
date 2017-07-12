
module DyNet.RNN (
    SimpleRNNBuilder,
    VanillaLSTMBuilder,
    createSimpleRNNBuilder,
    createVanillaLSTMBuilder,
    RNN(..)
) where


import DyNet.Core
import DyNet.Vector
import DyNet.Internal.RNN

class RNN a where
    newGraph :: a -> ComputationGraph -> Bool -> IO ()
    startNewSequence :: a -> [Expression] -> IO ()
    addInput :: a -> Expression -> IO Expression

    newGraph' :: a -> ComputationGraph -> IO ()
    newGraph' rnn cg = newGraph rnn cg True

    startNewSequence' :: a -> IO ()
    startNewSequence' rnn = startNewSequence rnn []

instance RNN SimpleRNNBuilder where
    newGraph = simpleRNNNewGraph
    startNewSequence rnn x = fromList x >>= (\x' -> simpleRNNStartNewSequence rnn x')
    addInput = simpleRNNAddInput

instance RNN VanillaLSTMBuilder where
    newGraph = vanillaLSTMNewGraph
    startNewSequence rnn x = fromList x >>= (\x' -> vanillaLSTMStartNewSequence rnn x')
    addInput = vanillaLSTMAddInput
