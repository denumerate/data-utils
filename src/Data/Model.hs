{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}
module Data.Model
  ( Model(..)
  , kfoldCV
  ) where

import Data.List(splitAt)
import Data.Text(Text)
import Numeric.LinearAlgebra.Data(Matrix,toRows,fromRows)
import Numeric.LinearAlgebra(Element)
import qualified Data.Vector as V
import Control.Monad.Random.Class(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.Except(MonadError)

-- |Describes a model class that describes a function, built from a data set,
-- that can then be used to predict new values on new data sets.
class Model m i o where
  -- |A set of parameters used to build a model.
  data ModelParams i o
  -- |Trains a model using a provided data set.
  trainModel :: MonadError Text me =>
    ModelParams i o
    -> Matrix i -- ^ The data set used to train the model.
    -> [o] -- ^ The output data.
    -> me (m i o) -- ^ The produced model.
  -- |Uses a data set to predict a new data set of values.
  predict :: MonadError Text me =>
    Matrix i -- ^ The data set to predict new values with.
    -> m i o -- ^ The model to use to predict.
    -> me [o] -- ^ The new values.
  -- |Takes a model, a data set, and uses an error function to produce a
  -- collection of error values.
  testModel :: MonadError Text me =>
    (o -> o -> a) -- ^ The error function.
    -> Matrix i -- ^ The data set being used.
    -> [o] -- ^ The associated output values.
    -> m i o -- ^ The model being tested.
    -> me [a] -- ^ The collected error values.
  testModel ef input output = fmap (zipWith ef output) . predict input

-- |Performs k-fold cross-validation on a model system using the supplied data.
kfoldCV :: forall m mr me a i o .
  (MonadRandom mr,MonadError Text me,Model m i o,Element i) =>
  (o -> o -> a) -> ModelParams i o -> Matrix i -> [o] -> Int
  -> mr (me [[a]])
kfoldCV ef params ins outs n =
  ((\ps -> let ps' = V.fromList ps in
       mapM (\i -> let (test,train) =
                         V.ifoldl' (\(a,b) i' val -> if i' == i
                                     then (val,b) else (a,val:b))
                         ([],[]) ps' in
                (trainModel params (fromRows $ concatMap (map fst) train)
                  (concatMap (map snd) train) :: me (m i o)) >>=
                testModel ef (fromRows $ map fst test) (map snd test))
       [0..n -1]) . splitN n) <$> shuffleM (zip (toRows ins) outs)

-- |Splits a list into n sub-lists.
splitN :: Int -> [a] -> [[a]]
splitN x = reverse . splitN' x
  where
    splitN' n ls
      |n <= 1 = [ls]
      |otherwise = let (fs,rst) = splitAt (div (length ls) n) ls in
                     fs : splitN' (n - 1) rst
