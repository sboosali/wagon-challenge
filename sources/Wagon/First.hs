{-| 


NOTES 

e.g. "sessionId (text)","page (text)","latency (number)","timeOnPage (number)"

constant schema? 

are all columns nullable?

are session IDs unique? is latency an integer? 

are all rows the same length as the schema? must they follow the schema at all? 



-}
{-# LANGUAGE LambdaCase, FlexibleContexts, BangPatterns #-} 
{-# OPTIONS_GHC -fno-warn-missing-signatures -funbox-strict-fields #-}
module Wagon.First where 

import qualified Control.Foldl as F 
import qualified Control.Lens as L 
import qualified Text.Parsec as P 
import qualified Pipes as I
import qualified Pipes.Prelude as I

import Control.Arrow ((>>>)) 
import Control.Monad (when) 
import System.IO (isEOF) 
import Control.Applicative(liftA2) 
import Data.Either (rights) 
import Data.Ord (compare) 
import Numeric.Natural (Natural) 
import Data.List (genericLength)
import Data.Function (on)
import System.Environment (getArgs)
import Data.Foldable (traverse_)


type Summary = Pair LineSummary RowSummary

data LineSummary = LineSummary
 { countInvalidSyntax :: Natural
 }
 deriving (Show,Read,Eq,Ord)

data Row_ sessionId page latency timeOnPage = Row_
 { sessionId  :: !sessionId
 , page       :: !page
 , latency    :: !latency 
 , timeOnPage :: !timeOnPage 
 } 
 deriving (Show,Read,Eq,Ord)

type RowValue = Row_
 (Maybe' String)  -- is it ever null? (no, it seems).
 (Maybe' String)
 (Maybe' Double)  -- is it always an integer? (yes, it seems).
 (Maybe' Double)

type RowSummary = Row_
 (Pair AnyRowSummary TextualRowSummary)
 (Pair AnyRowSummary TextualRowSummary)
 (Pair AnyRowSummary NumericRowSummary)
 (Pair AnyRowSummary NumericRowSummary)

data AnyRowSummary = AnyRowSummary
 { anyTotalCount :: !Int
 , anyNullCount  :: !Int
 }
 deriving (Show,Read,Eq,Ord)

data NumericRowSummary = NumericRowSummary
 { numericMinimum  :: !(Maybe' Double)
 , numericMaximum  :: !(Maybe' Double)
 , numericAverage  :: !Double
 }
 deriving (Show,Read,Eq,Ord)

data TextualRowSummary = TextualRowSummary
 { textualCountShortest :: !(Maybe' (Pair Natural Natural))
 , textualCountLongest  :: !(Maybe' (Pair Natural Natural))
 , textualAverageLength :: !Double
 }
 deriving (Show,Read,Eq,Ord)


type P = P.Parsec String () 

parse p = P.parse (p <* P.eof) "" 
{-# INLINEABLE parse #-}

parseRow = parse pRow
{-# INLINEABLE parseRow #-}

-- parseRow s = case parse pRow s of 
--  Right a -> Right a 
--  Left e -> Left (s, e) 

pRow :: P RowValue 
pRow = Row_
 <$> fmap Just' pText 
 <*  pSeparator 
 <*> optionMaybe' pText 
 <*  pSeparator 
 <*> optionMaybe' pInteger 
 <*  pSeparator 
 <*> optionMaybe' pDouble 
{-# INLINEABLE pRow #-}

optionMaybe' p = toMaybe' <$> P.optionMaybe p
{-# INLINEABLE optionMaybe' #-}

pText = P.many1 P.alphaNum
{-# INLINEABLE pText #-}

pInteger = read <$> pDigits            -- is total 
{-# INLINEABLE pInteger #-}

pDouble = read <$>            -- should be total 
 (append3 <$> pDigits
          <*> P.string "."
          <*> pDigits)
 where 
 append3 x y z = x++y++z 
{-# INLINEABLE pDouble #-}

pDigits = P.many1 P.digit 
{-# INLINEABLE pDigits #-}

pSeparator = P.string ","
{-# INLINEABLE pSeparator #-}


summarize :: Foldable t => t String -> Summary
summarize = F.fold fSummary

summarizeRows :: Foldable t => t RowValue -> RowSummary 
summarizeRows = F.fold fRowSummary

countStdin = do
 k <- F.fold F.length <$> getLines
 print k

summarizeStdinStreaming :: IO () -- doesn't leak
summarizeStdinStreaming = do
 _ <- getLine
 print =<< foldI (not <$> isEOF) (stripLine <$> getLine) fSummary

foldI :: (Monad m) => m Bool -> m a -> F.Fold a b -> m b
foldI isGood getNext (F.Fold step begin finish) = finish <$> go begin
 where
 go !s = do
  c <- isGood
  if   c
  then do
    a <- getNext
    go (step s a) -- tail-recursive
  else return s

summarizeStdinPipes :: IO () -- doesn't leak
summarizeStdinPipes = do
 print =<< F.purely I.fold fSummary (I.stdinLn I.>-> I.drop 1) 

summarizeStdinLazily :: IO () -- leaks
summarizeStdinLazily = do
 getLines >>= (tail >>> summarize >>> print)

summarizeStdin :: Bool -> IO () -- leaks
summarizeStdin shouldEcho = do 
 getLines >>= \case
  [] -> return ()

  (header:contents) -> do

      if shouldEcho
      then do
          putStrLn "" 
          traverse_ print contents

          putStrLn "" 
          traverse_ print (fmap parseRow contents)
      else return()

      putStrLn "" 
      print $ summarize contents -- contents can't be streamed, traversing is too strict

fSummary :: F.Fold String Summary 
fSummary = F.premap parseRow $ Pair
  <$> fLineSummary
  <*> F.handles L._Right fRowSummary
{-# INLINEABLE fSummary #-}

fLineSummary = LineSummary <$> fCountLefts
{-# INLINEABLE fLineSummary #-}

fRowSummary :: F.Fold RowValue RowSummary 
fRowSummary = Row_
 <$> F.premap (sessionId  ) fSessionIdSummary 
 <*> F.premap (page       ) fPageSummary
 <*> F.premap (latency    ) fLatencySummary
 <*> F.premap (timeOnPage ) fTimeOnPageSummary
{-# INLINEABLE fRowSummary #-}

fSessionIdSummary  = (Pair <$> fAnyRowSummary <*> fTextualRowSummary)
{-# INLINEABLE fSessionIdSummary #-}

fPageSummary       = (Pair <$> fAnyRowSummary <*> fTextualRowSummary)
{-# INLINEABLE fPageSummary #-}

fLatencySummary    = (Pair <$> fAnyRowSummary <*> fNumericRowSummary)
{-# INLINEABLE fLatencySummary #-}

fTimeOnPageSummary = (Pair <$> fAnyRowSummary <*> fNumericRowSummary)
{-# INLINEABLE fTimeOnPageSummary #-}

fAnyRowSummary :: (Eq a) => F.Fold (Maybe' a) AnyRowSummary
fAnyRowSummary = AnyRowSummary
 <$> fAnyTotalCount 
 <*> fAnyNullCount  
{-# INLINEABLE fAnyRowSummary #-}

fNumericRowSummary :: F.Fold (Maybe' Double) NumericRowSummary
fNumericRowSummary = NumericRowSummary
 <$> fNumericMinimum  
 <*> fNumericMaximum
 <*> fNumericAverage
{-# INLINEABLE fNumericRowSummary #-}

fTextualRowSummary :: F.Fold (Maybe' String) TextualRowSummary
fTextualRowSummary = TextualRowSummary
 <$> fTextualCountShortest
 <*> fTextualCountLongest
 <*> fTextualAverageLength
{-# INLINEABLE fTextualRowSummary #-}

fAnyTotalCount :: F.Fold a Int
fAnyTotalCount = F.genericLength 
{-# INLINEABLE fAnyTotalCount #-}

fAnyNullCount :: (Eq a) => F.Fold (Maybe' a) Int
fAnyNullCount
 = F.premap (fromEnum . (==Nothing')) -- Nothing is True is 1
 $ F.sum 
{-# INLINEABLE fAnyNullCount #-}

fNumericMinimum
 = F.handles _Just'  -- ignore nulls
 . fmap toMaybe' 
 $ F.minimum
{-# INLINEABLE fNumericMinimum #-}

fNumericMaximum 
 = F.handles _Just'
 . fmap toMaybe' 
 $ F.maximum
{-# INLINEABLE fNumericMaximum #-}

-- both averaging summaries are partial via zero-division on Doubles, returning NaN on only-null columns
-- test with {./generator 1 | cabal run -- first --echo}
fNumericAverage
 = F.handles _Just'
 $ fAverage
{-# INLINEABLE fNumericAverage #-}

-- NOTE the wording was ambiguous (to me at least). I'll project the text into a number with length, filter for the shortest length, and then count them. one could also project after, to avoid adding together words, and merging the words somehow (least/alphabetically, first, last, all, etc.).
fTextualCountShortest
 = F.handles _Just'
 . F.premap genericLength
 $ fCountMonotone compare
{-# INLINEABLE fTextualCountShortest #-}

fTextualCountLongest
 = F.handles _Just'
 . F.premap genericLength
 $ fCountMonotone (down compare)
{-# INLINEABLE fTextualCountLongest #-}

fTextualAverageLength
 = F.handles _Just'
 . F.premap genericLength
 $ fAverage
{-# INLINEABLE fTextualAverageLength #-}

-- keep the smallest and its count
fCountMonotone :: (a -> a -> Ordering) -> F.Fold a (Maybe' (Pair a Natural))
fCountMonotone cmp = F.Fold go Nothing' id
 where
 go Nothing' a = Just' (Pair a 1)
 go (Just' a) b = Just' (inc a b)
 inc p@(Pair oldValue oldCount) newValue = case newValue `cmp` oldValue of
  LT -> Pair newValue 1
  EQ -> Pair oldValue (1 + oldCount)
  GT -> p
{-# INLINEABLE fCountMonotone #-}

fAverage :: (Fractional a) => F.Fold a a
fAverage = F.sum / F.genericLength  -- I love Haskell
{-# INLINEABLE fAverage #-}

fCountLefts :: F.Fold (Either e a) Natural
fCountLefts = F.handles L._Left F.genericLength
{-# INLINEABLE fCountLefts #-}


useLines k = do
 h <- getLine              -- ignore header, this schema is typed and we don't have dependent types or anything 
 interactClean (useLine k) 

useLine k = show . k . stripLine

interactClean k = whileM (not <$> isEOF) $ do
 s <- stripLine <$> getLine
 putStrLn $ k s 

whileM mb k = go 
 where
 go = do 
  b <- mb
  when b (k >> go) 

stripLine = filter (not . (`elem` "\n\r"))
 -- assumes the rows don't have any new lines 

getLines -- not lazy
 = (fmap stripLine . lines)
 <$> getContents

distributeEither :: [Either e a] -> Either e [a] 
distributeEither = traverse id 

down f = \x y -> case f x y of 
  LT -> GT
  EQ -> EQ
  GT -> LT


data Pair a b = Pair !a !b
 deriving (Show,Read,Eq,Ord)

data Maybe' a = Nothing' | Just' !a
 deriving (Show,Read,Eq,Ord)

_Just' :: L.Prism (Maybe' a) (Maybe' b) a b
_Just' = L.prism Just' $ \s -> case s of
 Nothing' -> Left Nothing' -- not s, as it must change type
 Just' a  -> Right a
{-# INLINEABLE _Just' #-}

toMaybe' :: Maybe a -> Maybe' a
toMaybe' = maybe Nothing' Just'
{-# INLINEABLE toMaybe' #-}


testRows_ = 
 [ "374b02ca,,,"
 , "b72327e1,query,83,378.089"
 , "b6a594f1,,10,"
 , "b8908aa0,explore,60,207.991"
 , "37897f61,query,17,375.851"
 , "3943a55a,welcome,56,30.105"
 , "b8c2ccf5,,10,"
 , "b9255260,query,142,340.757"
 , "38aa53e5,query,61,410.988"
 , "b64d35c4,welcome,43,42.091"
 ]

testRows = fmap parseRow testRows_

testRowsRight = rights testRows


main = do
 -- _ <- traverse print testRows
 -- useLines parseRow
 getArgs >>= \case
  ["--count"] -> countStdin
  ["--echo"] -> summarizeStdin True
  ["--naive"] -> summarizeStdin False
  ["--pipes"] -> summarizeStdinPipes
  _        -> summarizeStdinStreaming

