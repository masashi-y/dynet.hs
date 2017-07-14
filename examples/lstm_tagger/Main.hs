{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import System.Environment
import Control.Arrow ( (&&&) )
import Data.List ( sort, group, reverse, nub )
import Control.Monad ( join, foldM, mapM, forM_, forM, when )
import Control.Monad.Trans ( liftIO )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Options.Declarative as O

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


data Tagger = Tagger { vocab :: D.Dict
                     , labels :: D.Dict
                     , wembed :: D.LookupParameter
                     , pH :: D.Parameter
                     , pO :: D.Parameter
                     , fwdRNN :: D.VanillaLSTMBuilder
                     , bwdRNN :: D.VanillaLSTMBuilder }


createBiLSTMTagger :: D.Model ->
                      [Token] ->
                      [Label] ->
                      Int -> Int -> Int -> Int -> IO Tagger
createBiLSTMTagger m vocab labels layers wembedDim hiddenDim mlpDim =
    Tagger <$> D.createDict vocab (Just "<unk>")
           <*> D.createDict labels Nothing
           <*> D.addLookupParameters m vocabSize [wembedDim]
           <*> D.addParameters m [mlpDim, hiddenDim*2]
           <*> D.addParameters m [labelSize, mlpDim]
           <*> D.createVanillaLSTMBuilder layers wembedDim hiddenDim m False
           <*> D.createVanillaLSTMBuilder layers wembedDim hiddenDim m False
    where vocabSize = length vocab + 1
          labelSize = length labels


readData :: String -> IO ([[Token]], [[Label]])
readData fileName = do
    samples <- fmap T.lines $ T.readFile fileName
    return $ unzip $ map (unzip . (map split) . T.words) samples
    where split t = case T.splitOn "|" t of
              [word, tag] -> (word, tag)
              _ -> error $ "Failed to parse the input: " ++ T.unpack t


buildTaggingGraph :: D.ComputationGraph -> Tagger -> [Token] -> IO [D.Expression]
buildTaggingGraph cg (Tagger vocab _ wembed pH pO fwdRNN bwdRNN) input = do
    _H <- D.parameter cg pH
    _O <- D.parameter cg pO

    D.newGraph' fwdRNN cg
    D.newGraph' bwdRNN cg

    wembs <- forM input $ \w -> do
        i <- D.fromString vocab w
        D.lookup cg wembed i

    D.startNewSequence' fwdRNN
    fwds <- mapM (D.addInput fwdRNN) wembs

    D.startNewSequence' bwdRNN
    bwds <- reverse <$> mapM (D.addInput bwdRNN) (reverse wembs)

    res <- forM (zip fwds bwds) $ \(f, b) ->
        _O `D.mul` D.tanh ( _H `D.mul` D.concat' [f, b] )

    return res


sentLoss :: D.ComputationGraph -> Tagger -> [Token] -> [Label] -> IO D.Expression
sentLoss cg tagger input ys = do
    exprs <- buildTaggingGraph cg tagger input
    errs <- forM (zip exprs ys) $ \(expr, y) -> do
        y' <- D.fromString (labels tagger) y
        D.pickneglogsoftmax expr y'
    D.sum errs


tagSent :: Tagger -> [Token] -> IO [Label]
tagSent tagger input =
    D.withNewComputationGraph $ \cg -> do
        exprs <- buildTaggingGraph cg tagger input
        forM exprs $ \expr -> do
            v <- V.toList =<< D.asVector =<< D.forward cg expr
            D.fromIndex (labels tagger) (D.argmax v)


train :: D.Trainer t => t -> Tagger -> [[Token]] -> [[Label]] -> IO Float
train trainer tagger xs ys = do
    loss' <- forM (zip xs ys) $ \(x, y) ->
        D.withNewComputationGraph $ \cg -> do
            lossExp <- sentLoss cg tagger x y
            loss <- D.asScalar =<< D.forward cg lossExp
            D.backward cg lossExp
            D.update trainer 1.0
            return (loss, realToFrac $ length x)
    return $ (sum $ map fst loss') / (sum $ map snd loss')


makeBatch :: Int -> [a] -> [[a]]
makeBatch _    [] = []
makeBatch size xs = let (x, xs') = splitAt size xs in x:makeBatch size xs'


accuracy :: Eq a => [[a]] -> [[a]] -> Float
accuracy pred gold = realToFrac correct / realToFrac (length pred')
    where correct = length $ filter (\(p, g) -> p == g) $ zip pred' gold'
          pred' = join pred
          gold' = join gold


main' :: O.Flag "" '["iter"] "ITER" "iteration" (O.Def "30" Int)
      -> O.Flag "" '["layers"] "LAYERS" "stack N bi-LSTM(s)" (O.Def "1" Int)
      -> O.Flag "" '["wembed"] "WEMBED" "word embedding size" (O.Def "80" Int)
      -> O.Flag "" '["hidden"] "HIDDEN" "LSTM hidden vector size" (O.Def "100" Int)
      -> O.Flag "" '["mlp"] "MLP" "dimension of MLP after LSTMs" (O.Def "100" Int)
      -> O.Arg "TRAIN" (O.Req String)
      -> O.Arg "EVAL" (O.Req String)
      -> O.Cmd "train lstm tagger" ()
main' iter layers wembed hidden mlp trainData evalData = liftIO $ do
    (trainX, trainY) <- readData (O.get trainData)
    (evalX, evalY) <- readData (O.get evalData)
    let params@(layers', wembed', hidden', mlp') = (O.get layers, O.get wembed, O.get hidden, O.get mlp)
        vocab = foldl (\ws (w, c) -> if c > 5 then w:ws else ws) [] $ wordCount $ join trainX
        labels = nub $ join trainY

    putStrLn $ "(layers,embed_dim,hidden_dim,mlp_dim) = " ++ show params
    putStrLn $ "vocabulary size: " ++ show (length vocab)
    putStrLn $ "number of labels: " ++ show (length labels)

    m <- D.createModel
    trainer <- D.createAdamTrainer' m
    tagger <- createBiLSTMTagger m vocab labels layers' wembed' hidden' mlp'

    let batchX = makeBatch 500 trainX
        batchY = makeBatch 500 trainY
        evalCycle = min 8 (length batchX)

    forM_ [1..(O.get iter)] $  \_ ->
        forM_ (zip3 [1..] batchX batchY) $ \(i, xs, ys) -> do
            loss <- train trainer tagger xs ys
            D.status trainer
            print loss
            D.updateEpoch trainer 1.0
            when (i `mod` evalCycle == 0) $ do
                predY <- mapM (tagSent tagger) evalX
                putStrLn $ "accuracy: " ++ show (accuracy predY evalY)


main = do
    argv <- getArgs
    argv' <- D.initialize' argv
    O.run "lstm-tagger" argv' Nothing main'
