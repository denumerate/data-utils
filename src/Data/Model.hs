{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Model
  ( Model(..)
  , ErrorFunction
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
  -- |Trains a model using a provided data set.
  trainModel :: d i -- ^ The data set used to train the model.
    -> d o -- ^ The output data.
    -> m -- ^ The produced model.
  -- |Uses a data set to predict a new data set of values.
  predict :: d i -- ^ The data set to predict new values with.
    -> m -- ^ The model to use to predict.
    -> d o -- ^ The new values.
  -- |Takes a model, a data set, and uses an error function to produce a
  -- collection of error values.
  testModel :: ErrorFunction d t o a
    -> d i -- ^ The data set being used.
    -> d o -- ^ The associated output values.
    -> m -- ^ The model being tested.
    -> t a -- ^ The collected error values.
  testModel ef input output = ef output . predict input

-- |Basic operations required to work with a data set.
class DataSet d where
  -- |Splits a data set into two using a function.
  split :: (a -> (b,c)) -> d a -> (d b,d c)
  -- |Combines two data sets with the same record type.
  -- Allows for error throwing if the records do not match.
  append :: MonadError e m => d a -> d a -> m (d a)
  -- |Appends multiple data sets.
  concatSet :: MonadError e m => [d a] -> m (d a)
  concatSet vs = foldlM append (head vs) (tail vs)
  -- |Splits a data set into n random partitions.
  partition :: (MonadRandom m) => Int -> d a -> m (Vector (d a))

-- |A function that takes two data sets and produces a collection of error scores.
type ErrorFunction d t a b = d a -> d a -> t b

kfoldCV :: forall m d mr e me t a i o b .
  (DataSet d,MonadRandom mr,MonadError e me,Model m d i o) =>
  ErrorFunction d t o a -> (b -> (i,o)) -> d b -> Int -> mr (me [t a])
kfoldCV ef sf dset n = partition n dset >>=
  \ds -> return $ mapM (\i -> concatSet (V.toList $
                                         V.ifilter (\i' _ -> i'/=i) ds) >>=
                         \ds' ->
                           return (let (ia,oa) = split sf ds'
                                       (ib,ob) = split sf (ds V.! i) in
                                      testModel ef ib ob
                                      (trainModel ia oa :: m)))
         [0..n]
