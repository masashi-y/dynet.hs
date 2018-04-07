{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Data.List ( intercalate )
import Control.Monad.Trans ( liftIO )
import Text.Parsec
import Control.Monad
import System.Environment
import qualified Data.Text as T
import qualified Options.Declarative as O

import qualified DyNet.Core as D
import qualified DyNet.Expr as D
import qualified DyNet.RNN as D
import qualified DyNet.Dict as D
import qualified DyNet.Train as D
import qualified DyNet.Vector as V

import Utils ( wordCount, accuracy, makeBatch )

type Token = T.Text
type Label = Int
type H = D.Expression
type C = D.Expression

data Tree a = Node { get :: a, children :: [Tree a] }
            | Leaf { word :: Token }


getNonTerms :: Tree a -> [a]
getNonTerms (Node v children) = v:concatMap getNonTerms children
getNonTerms (Leaf _) = []


getTerms :: Tree a -> [Token]
getTerms (Node _ children) = concatMap getTerms children
getTerms (Leaf w) = [w]


instance Show a => Show (Tree a) where
    show (Node label children) = "(" ++ show label ++ " " ++ intercalate " " (map show children) ++ ")"
    show (Leaf word) = T.unpack word


score :: Parsec String st Int
score = read . return <$> digit


leaf :: Parsec String st (Tree Label)
leaf = Node <$> score <* space <*> word'
    where word' = return . Leaf . T.pack <$> many1 (noneOf "() ")


node :: Parsec String st (Tree Label)
node = Node <$> score <* space <*> (tree `sepBy1` (char ' '))


tree :: Parsec String st (Tree Label)
tree = between (char '(') (char ')') (try leaf <|> node)


readTrees :: String -> IO [Tree Label]
readTrees fileName = do
    contents <- lines <$> readFile fileName
    return $ map parse' contents
    where parse' line = case parse tree "" line of
              Right c -> c
              Left e -> error $ show e


data TreeLSTM = TreeLSTM { vocab :: D.Dict Token
                         , pE :: D.LookupParameter
                         , pWS :: [D.Parameter]
                         , pUS :: [D.Parameter]
                         , pUFS :: [D.Parameter]
                         , pBS :: [D.Parameter] }


createTreeLSTM :: D.Model -> [Token] -> Int -> Int -> IO TreeLSTM
createTreeLSTM m vocab wdim hdim =
    TreeLSTM <$> D.createDict vocab (Just "<unk>")
             <*> D.addLookupParameters' m vocabSize [wdim]
             <*> sequence [D.addParameters' m [hdim, wdim] | _ <- "iou"]
             <*> sequence [D.addParameters' m [hdim, hdim * 2] | _ <- "iou"]
             <*> sequence [D.addParameters' m [hdim, hdim] | _ <- "ff"]
             <*> sequence [D.addParameters' m [hdim] | _ <- "iouf"]
    where vocabSize = length vocab + 1


exprForTree :: D.ComputationGraph -> TreeLSTM -> (Tree Label) -> IO (Tree (H, C))
exprForTree cg (TreeLSTM vocab pE pWS _ _ pBS) (Node _ [Leaf word]) = do
    emb <- D.lookup cg pE =<< D.fromString vocab word
    [_Wi, _Wo, _Wu] <- mapM (D.parameter cg) pWS
    [bi, bo, bu, _] <- mapM (D.parameter cg) pBS
    i <- D.logistic $ D.affineTransform [bi, _Wi, emb]
    o <- D.logistic $ D.affineTransform [bo, _Wo, emb]
    u <- D.tanh     $ D.affineTransform [bu, _Wu, emb]
    c <- D.cmult i u
    h <- D.cmult o (D.tanh c)
    return $ Node (h, c) []
exprForTree cg lstm@(TreeLSTM _ _ _ pUS pUFS pBS) (Node _ [ch1, ch2]) = do
    n1 <- exprForTree cg lstm ch1
    n2 <- exprForTree cg lstm ch2
    let ((h1, c1), (h2, c2)) = (get n1, get n2)
    [_Ui, _Uo, _Uu] <- mapM (D.parameter cg) pUS
    [_Uf1, _Uf2] <- mapM (D.parameter cg) pUFS
    [bi, bo, bu, bf] <- mapM (D.parameter cg) pBS
    e <- D.concat' [h1, h2]
    i  <- D.logistic $ D.affineTransform [bi, _Ui, e]
    o  <- D.logistic $ D.affineTransform [bo, _Uo, e]
    f1 <- D.logistic $ D.affineTransform [bf, _Uf1, h1]
    f2 <- D.logistic $ D.affineTransform [bf, _Uf2, h2]
    u  <- D.tanh $ D.affineTransform [bu, _Uu, e]
    c  <- D.cmult i u `D.add` D.cmult f1 c1 `D.add` D.cmult f2 c2
    h  <- D.cmult o (D.tanh c)
    return $ Node (h, c) [n1, n2]


train :: D.Trainer t => t -> D.Parameter ->
    TreeLSTM -> [Tree Label] -> [[Label]] -> IO Float
train trainer pW lstm xs ys = do
    loss' <- forM (zip xs ys) $ \(x, y) ->
        D.withNewComputationGraph $ \cg -> do
            _W <- D.parameter cg pW
            y_pred <- getNonTerms <$> exprForTree cg lstm x
            losses <- forM  (zip y y_pred) $ \(y', y_pred') -> do
                D.pickneglogsoftmax (_W `D.mul` (fst y_pred')) y'
            lossExp <- D.sum losses
            loss <- D.asScalar =<< D.forward cg lossExp
            D.backward cg lossExp
            D.update trainer
            return (loss, realToFrac $ length y)
    return $ (sum $ map fst loss') / (sum $ map snd loss')


main' :: O.Flag "" '["iter"] "ITER" "iteration" (O.Def "30" Int)
      -> O.Flag "" '["wembed"] "WEMBED" "word embedding size" (O.Def "80" Int)
      -> O.Flag "" '["hidden"] "HIDDEN" "LSTM hidden vector size" (O.Def "100" Int)
      -> O.Arg "TRAIN" (O.Req String)
      -> O.Arg "EVAL" (O.Req String)
      -> O.Cmd "train lstm tagger" ()
main' iter wembed hidden trainData evalData = liftIO $ do
    trainX <- readTrees (O.get trainData)
    evalX  <- readTrees (O.get evalData)
    let trainY = map getNonTerms trainX
        evalY  = map (return . get) evalX
        params@(wembed', hidden') = (O.get wembed, O.get hidden)
        vocab = foldl (\ws (w, c) -> if c > 5 then w:ws else ws) [] $ wordCount $ join $ (map getTerms trainX)
        labels = 5

    putStrLn $ "(embed_dim,hidden_dim) = " ++ show params
    putStrLn $ "vocabulary size: " ++ show (length vocab)
    putStrLn $ "number of labels: " ++ show labels

    m <- D.createModel
    pW <- D.addParameters' m [labels, hidden']
    trainer <- D.createAdamTrainer' m
    lstm <- createTreeLSTM m vocab wembed' hidden'

    let batchX = makeBatch 500 trainX
        batchY = makeBatch 500 trainY
        evalCycle = min 8 (length batchX)

    forM_ [1..(O.get iter)] $  \_ ->
        forM_ (zip3 [1..] batchX batchY) $ \(i, xs, ys) -> do
            loss <- train trainer pW lstm xs ys
            D.status trainer
            print loss
            when (i `mod` evalCycle == 0) $ do
                predY <- forM evalX $ \x -> do
                    D.withNewComputationGraph $ \cg -> do
                        _W <- D.parameter cg pW
                        (h, _) <- get <$> exprForTree cg lstm x
                        expr <- _W `D.mul` h
                        res <- V.toList =<< D.asVector =<< D.forward cg expr
                        return $ [D.argmax res]
                putStrLn $ "accuracy: " ++ show (accuracy predY evalY)


main = do
    argv <- getArgs
    argv' <- D.initialize' argv
    O.run "treelstm" argv' Nothing main'
