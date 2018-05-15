{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
module Data.Model
  ( Model
  , ModelM
  , kfoldCV
  , kfoldCVM
  ) where

import Data.List(splitAt)
import Numeric.LinearAlgebra.Data(Matrix,toRows,fromRows,fromColumns)
import Numeric.LinearAlgebra(Element)
import qualified Data.Vector as V
import Control.Monad.Random.Class(MonadRandom)
import System.Random.Shuffle(shuffleM)

-- |In their simplest form, models are just functions that take one matrix
-- and return another.
type Model i o = Matrix i -> Matrix o

-- |Monadic models, allowing failure, etc.
type ModelM m i o = Matrix i -> m (Matrix o)

-- |Performs k-fold cross-validation on a model system using the supplied data.
kfoldCV :: forall mr a i o .
  (MonadRandom mr,Element i,Element o) =>
  (Matrix i -> Matrix o -> Model i o) ->
  (Matrix o -> Matrix o -> Matrix a) -> Matrix i -> Matrix o -> Int
  -> mr [Matrix a]
kfoldCV bf ef ins outs n =
  ((\ps -> let ps' = V.fromList ps in
       map (\i -> let (test,train) =
                        V.ifoldl' (\(a,b) i' val -> if i' == i
                                     then (val,b) else (a,val:b))
                         ([],[]) ps'
                      train' = concat $ reverse train in
                ef (fromColumns $ map snd test) $
                bf (fromRows $ map fst train') (fromRows $ map snd train') $
                fromRows $ map fst test)
       [0..n -1]) . splitN n) <$> shuffleM (zip (toRows ins) (toRows outs))

-- |Performs k-fold cross-validation on a model system using the supplied data.
kfoldCVM :: forall m a i o .
  (MonadRandom m,Element i,Element o) =>
  (Matrix i -> Matrix o -> ModelM m i o) ->
  (Matrix o -> Matrix o -> Matrix a) -> Matrix i -> Matrix o -> Int
  -> m [Matrix a]
kfoldCVM bf ef ins outs n =
  shuffleM (zip (toRows ins) (toRows outs)) >>=
  ((\ps -> let ps' = V.fromList ps in
       mapM (\i -> let (test,train) =
                        V.ifoldl' (\(a,b) i' val -> if i' == i
                                     then (val,b) else (a,val:b))
                         ([],[]) ps'
                       train' = concat $ reverse train in
                ef (fromColumns $ map snd test) <$>
                bf (fromRows $ map fst train') (fromRows $ map snd train')
                (fromRows $ map fst test))
       [0..n -1]) . splitN n)

-- |Splits a list into n sub-lists.
splitN :: Int -> [a] -> [[a]]
splitN x = reverse . splitN' x
  where
    splitN' n ls
      |n <= 1 = [ls]
      |otherwise = let (fs,rst) = splitAt (div (length ls) n) ls in
                     fs : splitN' (n - 1) rst
