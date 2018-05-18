{-# LANGUAGE ScopedTypeVariables, MultiWayIf #-}
module Data.Read
  ( readCSV
  , readCSV2
  ) where

import System.IO(IOMode(..),FilePath,hGetContents,openFile,hClose)
import Text.Parsec(parserFail,(<|>),parse,ParseError)
import Text.Parsec.Combinator(many1,sepBy,sepEndBy)
import Text.Parsec.String(Parser)
import Text.Parsec.Char(noneOf,char,crlf)
import Text.Read(readMaybe)
import Numeric.LinearAlgebra(Element)
import Numeric.LinearAlgebra.Data(Matrix,fromRows)
import qualified Numeric.LinearAlgebra.Data as LN
import Control.Monad(foldM)

-- |Reads a csv, constructing a matrix from the specified columns
readCSV :: (Read a,Element a) => FilePath -> [Int]
  -> IO (Either ParseError (Matrix a))
readCSV fpath is = openFile fpath ReadMode >>=
  \handle -> fmap (parse (parseCSV (parseLine is)) "CSV")
  (hGetContents handle) >>=
  \val ->
    hClose handle >>
    case val of
      Left e -> return $ Left e
      Right ls -> return $ Right $ fromRows $ map LN.fromList ls

-- |Reads a csv, constructing two matrices from the specified columns
readCSV2 :: (Read a,Element a,Read b,Element b) => FilePath -> [Int] -> [Int]
  -> IO (Either ParseError (Matrix a,Matrix b))
readCSV2 fpath as bs = openFile fpath ReadMode >>=
  \handle -> fmap (parse (parseCSV (parseLine2 as bs)) "CSV")
  (hGetContents handle) >>=
  \val ->
    hClose handle >>
    case val of
      Left e -> return $ Left e
      Right ls -> let m1 = fromRows $ map (LN.fromList . fst) ls
                      m2 = fromRows $ map (LN.fromList . snd) ls in
                    return $ Right (m1,m2)

-- |Uses a line parser to parse a csv
parseCSV :: Parser a -> Parser [a]
parseCSV p = sepEndBy p (crlf <|> char '\n' <|> char '\r')

-- |Parses a line of a csv, extracting the asked for cells
parseLine :: forall a . Read a => [Int] -> Parser [a]
parseLine is =
  sepBy (many1 (noneOf [',','\n','r'])) (char ',') >>=
  \cs -> case mapM (\(x,_) -> (readMaybe x :: Maybe a)) $
              filter (\(_,i) -> elem i is) $ zip cs [0..] of
           Just cs' -> return cs'
           _ -> parserFail "Read Failed"

-- |Parses a line of a csv, extracting the asked for cells into their respective
-- lists.
-- Assumes no overlap between the cells being asked for.
parseLine2 :: forall a b . (Read a,Read b) => [Int] -> [Int] -> Parser ([a],[b])
parseLine2 as bs =
  sepBy (many1 (noneOf [',','\n','r'])) (char ',') >>=
  \cs -> case foldM (\acc@(as',bs') (x,i) ->
                       if
                         |elem i as -> (\x' -> (x':as',bs')) <$>
                                       (readMaybe x :: Maybe a)
                         |elem i bs -> (\x' -> (as',x':bs')) <$>
                                       (readMaybe x :: Maybe b)
                         |otherwise -> Just acc) ([],[]) $
              zip cs [0..] of
           Just (x,y) -> return (reverse x,reverse y)
           _ -> parserFail "Read Failed"
