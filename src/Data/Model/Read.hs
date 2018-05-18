{-# LANGUAGE ScopedTypeVariables, MultiWayIf #-}
module Data.Read
  ( readCSV
  , readCSV2
  ) where

import System.IO(IOMode(..),FilePath,hGetContents,openFile,hClose)
import Text.Parsec(parserFail,(<|>),parse,ParseError,many,string,try,(<?>))
import Text.Parsec.Combinator(sepBy,endBy)
import Text.Parsec.String(Parser)
import Text.Parsec.Char(noneOf,char,crlf)
import Text.Read(readMaybe)
import Numeric.LinearAlgebra(Element)
import Numeric.LinearAlgebra.Data(Matrix,fromRows)
import qualified Numeric.LinearAlgebra.Data as LN
import Control.Monad(foldM)

-- |Reads a csv, constructing a matrix from the specified columns
readCSV :: (Read a,Element b) => (a -> b) -> FilePath -> [Int]
  -> IO (Either ParseError (Matrix b))
readCSV f fpath is = openFile fpath ReadMode >>=
  \handle -> fmap (parse (parseCSV (parseLine f is)) "CSV")
  (hGetContents handle) >>=
  \val ->
    hClose handle >>
    case val of
      Left e -> return $ Left e
      Right ls -> return $ Right $ fromRows $ map LN.fromList ls

-- |Reads a csv, constructing two matrices from the specified columns
readCSV2 :: (Read a,Element c,Read b,Element d) =>
  (a -> c) -> (b -> d) -> FilePath -> [Int] -> [Int]
  -> IO (Either ParseError (Matrix c,Matrix d))
readCSV2 fa fb fpath as bs = openFile fpath ReadMode >>=
  \handle -> fmap (parse (parseCSV (parseLine2 fa fb as bs)) "CSV")
  (hGetContents handle) >>=
  \val ->
    case val of
      Left e -> hClose handle >>
                return (Left e)
      Right ls -> hClose handle >>
                  let m1 = fromRows $ map (LN.fromList . fst) ls
                      m2 = fromRows $ map (LN.fromList . snd) ls in
                    return $ Right (m1,m2)

-- |Uses a line parser to parse a csv
parseCSV :: Parser a -> Parser [a]
parseCSV p = endBy p eol

-- |Parses a line of a csv, extracting the asked for cells
parseLine :: forall a b . Read a => (a -> b) -> [Int] -> Parser [b]
parseLine f is =
  sepBy (many (noneOf ",\n\r")) (char ',') >>=
  \cs -> case mapM (\(x,_) -> f <$> (readMaybe x :: Maybe a)) $
              filter (\(_,i) -> elem i is) $ zip cs [0..] of
           Just cs' -> return cs'
           _ -> parserFail "Read Failed"

-- |Parses a line of a csv, extracting the asked for cells into their respective
-- lists.
-- Assumes no overlap between the cells being asked for.
parseLine2 :: forall a b c d . (Read a,Read b) =>
  (a -> c) -> (b -> d) -> [Int] -> [Int] -> Parser ([c],[d])
parseLine2 fa fb as bs =
  sepBy (many (noneOf ",\r\n")) (char ',') >>=
  \cs -> case foldM (\acc@(as',bs') (x,i) ->
                       if
                         |elem i as -> (\x' -> (fa x':as',bs')) <$>
                                       (readMaybe x :: Maybe a)
                         |elem i bs -> (\x' -> (as',fb x':bs')) <$>
                                       (readMaybe x :: Maybe b)
                         |otherwise -> Just acc) ([],[]) $
              zip cs [0..] of
           Just (x,y) -> return (reverse x,reverse y)
           _ -> parserFail "Read Failed"

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
