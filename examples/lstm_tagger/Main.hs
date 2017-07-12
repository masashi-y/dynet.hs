{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ( (&&&) )
import Data.List ( sort, group, reverse )
import Control.Monad ( join, foldM, mapM, forM )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

import qualified DyNet.Core as D
import qualified DyNet.Expr as D
import qualified DyNet.RNN as D
import qualified DyNet.Dict as D
import qualified DyNet.Train as D
import qualified DyNet.Vector as V


type Label = T.Text
type Token = T.Text

wordCount :: [Token] -> [(Token, Int)]
wordCount = map (head &&& length) . group . sort

data BiLSTMTagger = Tagger { dict :: D.Dict
                           , wordLookup :: D.LookupParameter
                           , pH :: D.Parameter
                           , pO :: D.Parameter
                           , fwdRNN :: D.VanillaLSTMBuilder
                           , bwdRNN :: D.VanillaLSTMBuilder }

readData :: String -> IO [([Token], [Label])]
readData fileName = do
    samples <- fmap T.lines $ T.readFile fileName
    return $ map (unzip . (map split) . T.words) samples
    where split t = case T.splitOn "|" t of
              [word, tag] -> (word, tag)
              _ -> error $ "Failed to parse the input: " ++ T.unpack t


wordRep :: D.Dict -> Token -> IO D.Expression
wordRep dict word = undefined

buildTaggingGraph :: D.ComputationGraph ->
                     BiLSTMTagger ->
                     [Token] ->
                     IO [D.Expression]
buildTaggingGraph cg (Tagger dict wordLookup pH pO fwdRNN bwdRNN) input = do
    _H <- D.parameter cg pH
    _O <- D.parameter cg pO

    D.newGraph' fwdRNN cg
    D.newGraph' bwdRNN cg

    wembs <- mapM (wordRep dict) input

    D.startNewSequence' fwdRNN
    fwds <- mapM (D.addInput fwdRNN) wembs

    D.startNewSequence' bwdRNN
    bwds <- mapM (D.addInput fwdRNN) (reverse wembs)

    res <- forM (zip fwds bwds) $ \(f, b) ->
        _O `D.mul` D.tanh ( _H `D.mul` D.concat [f, b] 0 )

    return res

sentLoss :: D.ComputationGraph ->
            BiLSTMTagger ->
            [Token] ->
            [Label] ->
            IO D.Expression
sentLoss cg tagger input labels = do
    exprs <- buildTaggingGraph cg tagger input
    errs <- forM (zip exprs labels) $ \(x, y) ->
        D.pickneglogsoftmax x =<< convert labelDict y
    D.sum errs

main = do
    -- trainData <- readData "data/dev.txt"
    -- dict <- createDict $ join $ map fst trainData
    -- res <- H.lookup dict "test"
    -- print ""
    let hiddenSize = 8
        iteration = 30
    argv <- getArgs
    D.initialize argv True
    m <- D.createModel
    trainer <- D.createSimpleSGDTrainer m 0.1 0.0

    p_W1 <- D.addParameters m [hiddenSize]
    p_W2 <- D.addParameters m [hiddenSize]
    p_W3 <- D.addParameters m [hiddenSize]
    p_W4 <- D.addParameters m [hiddenSize]
    p_W5 <- D.addParameters m [hiddenSize]
    p_W6 <- D.addParameters m [hiddenSize]
    p_W7 <- D.addParameters m [hiddenSize]
    p_W8 <- D.addParameters m [hiddenSize]

    cg <- D.createComputationGraph

    _W1 <- D.input cg [8] ([0,0,0,0,0,0,0,0] :: [Float])
    _W2 <- D.parameter cg p_W2
    _W3 <- D.parameter cg p_W3
    _W4 <- D.parameter cg p_W4
    _W5 <- D.parameter cg p_W5
    _W6 <- D.parameter cg p_W6
    _W7 <- D.parameter cg p_W7
    _W8 <- D.parameter cg p_W8

    lstm <- D.createVanillaLSTMBuilder 2 8 10 m False

    D.newGraph' lstm cg
    D.startNewSequence' lstm

    fwds <- mapM (D.addInput lstm) [_W1, _W2, _W3, _W4, _W5, _W6, _W7, _W8]

    return ()
