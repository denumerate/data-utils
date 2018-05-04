{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Model
  ( Model(..)
  , DataSet(..)
  , kfoldCV
  ) where

import Data.Foldable(foldlM)
import Data.Vector(Vector)
import qualified Data.Vector as V
import Control.Monad.Random.Class(MonadRandom)
import Control.Monad.Except(MonadError)

-- |Describes a model class that describes a function, built from a data set,
-- that can then be used to predict new values on new data sets.
class DataSet d => Model m d i o where
  -- |A set of parameters used to build a model.
  data ModelParams i o
  -- |Trains a model using a provided data set.
  trainModel :: ModelParams i o
    -> d i -- ^ The data set used to train the model.
    -> d o -- ^ The output data.
    -> m i o -- ^ The produced model.
  -- |Uses a data set to predict a new data set of values.
  predict :: d i -- ^ The data set to predict new values with.
    -> m i o -- ^ The model to use to predict.
    -> d o -- ^ The new values.
  -- |Takes a model, a data set, and uses an error function to produce a
  -- collection of error values.
  testModel :: (o -> o -> a) -- ^ The error function.
    -> d i -- ^ The data set being used.
    -> d o -- ^ The associated output values.
    -> m i o -- ^ The model being tested.
    -> d a -- ^ The collected error values.
  testModel ef input output = combineBy ef output . predict input

-- |Basic operations required to work with a data set.
class DataSet d where
  -- |Splits a data set into two using a function.
  splitBy :: (a -> (b,c)) -> d a -> (d b,d c)
  -- |Combines two data sets using a function.
  combineBy :: (a -> b -> c) -> d a -> d b -> d c
  -- |Combines two data sets with the same record type.
  -- Allows for error throwing if the records do not match.
  append :: MonadError e m => d a -> d a -> m (d a)
  -- |Appends multiple data sets.
  concatSet :: MonadError e m => [d a] -> m (d a)
  concatSet vs = foldlM append (head vs) (tail vs)
  -- |Splits a data set into n random partitions.
  partition :: (MonadRandom m) => Int -> d a -> m (Vector (d a))

kfoldCV :: forall m d mr e me a i o b .
  (DataSet d,MonadRandom mr,MonadError e me,Model m d i o) =>
  (o -> o -> a) -> (b -> (i,o)) -> ModelParams i o -> d b -> Int -> mr (me [d a])
kfoldCV ef sf ps dset n = partition n dset >>=
  \ds -> return $ mapM (\i -> concatSet (V.toList $
                                         V.ifilter (\i' _ -> i'/=i) ds) >>=
                         \ds' ->
                           return (let (ia,oa) = splitBy sf ds'
                                       (ib,ob) = splitBy sf (ds V.! i) in
                                      testModel ef ib ob
                                      (trainModel ps ia oa :: m i o)))
         [0..n]
