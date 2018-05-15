{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}
module Data.Model
  ( Model(..)
  -- , kfoldCV
  ) where

import Data.List(splitAt)
import Data.Text(Text)
import Numeric.LinearAlgebra.Data(Matrix,toRows,fromRows)
import Numeric.LinearAlgebra(Element)
import qualified Data.Vector as V
import Control.Monad.Random.Class(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.Except(MonadError)

type ModelFunc m i o = Matrix i -> m [o]

-- |Describes a model class that describes a function, built from a data set,
-- that can then be used to predict new values on new data sets.
-- All models are monadic because they, at minimum have to be allowed to fail.
class Monad m => Model m i o where
  -- |A set of parameters used to build a model.
  data ModelParams i o
  -- |Trains a model using a provided data set.
  buildModel ::
    ModelParams i o
    -> Matrix i -- ^ The data set used to train the model.
    -> [o] -- ^ The output data.
    -> ModelFunc m i o -- ^ The produced model.
  testModel ::
    ModelFunc m i o -- ^ The model being tested.
    -> (o -> o -> a) -- ^ The error function.
    -> Matrix i -- ^ The data set being used.
    -> [o] -- ^ The associated output values.
    -> m [a] -- ^ The collected error values.
  testModel m ef input output = zipWith ef output <$> m input

-- -- |Performs k-fold cross-validation on a model system using the supplied data.
-- kfoldCV :: forall m mr me a i o .
--   (MonadRandom mr,MonadError Text me,Model m i o,Element i) =>
--   (o -> o -> a) -> ModelParams i o -> Matrix i -> [o] -> Int
--   -> mr (me [[a]])
-- kfoldCV ef params ins outs n =
--   ((\ps -> let ps' = V.fromList ps in
--        mapM (\i -> let (test,train) =
--                          V.ifoldl' (\(a,b) i' val -> if i' == i
--                                      then (val,b) else (a,val:b))
--                          ([],[]) ps' in
--                 (trainModel params (fromRows $ concatMap (map fst) train)
--                   (concatMap (map snd) train) :: me (m i o)) >>=
--                 testModel ef (fromRows $ map fst test) (map snd test))
--        [0..n -1]) . splitN n) <$> shuffleM (zip (toRows ins) outs)

-- -- |Performs k-fold cross-validation on a model system using the supplied data.
-- kfoldCVM :: forall m mr me a i o .
--   (MonadRandom mr,MonadError Text me,ModelM mr m i o,Element i) =>
--   (o -> o -> a) -> ModelParams i o -> Matrix i -> [o] -> Int
--   -> mr (me [[a]])
-- kfoldCVM ef params ins outs n =
--   shuffleM (zip (toRows ins) outs) >>=
--   ((\ps -> let ps' = V.fromList ps in
--        mapM (\i -> let (test,train) =
--                          V.ifoldl' (\(a,b) i' val -> if i' == i
--                                      then (val,b) else (a,val:b))
--                          ([],[]) ps' in
--                 (trainModelM params (fromRows $ concatMap (map fst) train)
--                   (concatMap (map snd) train) :: mr (me (m i o))) >>=
--                 \a -> a >>=
--                       testModelM ef (fromRows $ map fst test) (map snd test))
--        [0..n -1]) . splitN n)

-- |Splits a list into n sub-lists.
splitN :: Int -> [a] -> [[a]]
splitN x = reverse . splitN' x
  where
    splitN' n ls
      |n <= 1 = [ls]
      |otherwise = let (fs,rst) = splitAt (div (length ls) n) ls in
                     fs : splitN' (n - 1) rst
