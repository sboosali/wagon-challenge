{-| 


installing:

$ git clone https://github.com/sboosali/wagon && cd wagon
$ cabal update &&  cabal sandbox init && cabal install -j4 --enable-library-profiling --enable-executable-profiling


building: 

$ cabal configure --enable-library-profiling --enable-executable-profiling && cabal build 


running: 

$ NUMBER=100000
$ ./generator $NUMBER | cabal run first 


profiling:

cabal configure --enable-library-profiling --enable-executable-profiling && cabal build && (./generator 100000 | cabal run first) && hp2ps -e8in -c first.hp && cat first.prof && open first.ps


timing:


$ time (./generator 10000 | cabal run first)

(LineSummary {countInvalidSyntax = 0},Row_ {sessionId = (AnyRowSummary {anyTotalCount = 10000, anyNullCount = 0},TextualRowSummary {textualCountShortest = Just (8,10000), textualCountLongest = Just (8,10000), textualAverageLength = 8.0}), page = (AnyRowSummary {anyTotalCount = 10000, anyNullCount = 757},TextualRowSummary {textualCountShortest = Just (5,4718), textualCountLongest = Just (7,4525), textualAverageLength = 5.979119333549713}), latency = (AnyRowSummary {anyTotalCount = 10000, anyNullCount = 0},NumericRowSummary {numericMinimum = Just 4.0, numericMaximum = Just 1040.0, numericAverage = 78.6968}), timeOnPage = (AnyRowSummary {anyTotalCount = 10000, anyNullCount = 757},NumericRowSummary {numericMinimum = Just 0.467, numericMaximum = Just 582.517, numericAverage = 204.89528324137163})})

real	0m1.583s


$ time (./generator 1000000 | cabal run first)

(LineSummary {countInvalidSyntax = 0},Row_ {sessionId = (AnyRowSummary {anyTotalCount = 1000000, anyNullCount = 0},TextualRowSummary {textualCountShortest = Just (8,1000000), textualCountLongest = Just (8,1000000), textualAverageLength = 8.0}), page = (AnyRowSummary {anyTotalCount = 1000000, anyNullCount = 79609},TextualRowSummary {textualCountShortest = Just (5,461113), textualCountLongest = Just (7,459278), textualAverageLength = 5.998006282112711}), latency = (AnyRowSummary {anyTotalCount = 1000000, anyNullCount = 0},NumericRowSummary {numericMinimum = Just 4.0, numericMaximum = Just 1334.0, numericAverage = 77.186638}), timeOnPage = (AnyRowSummary {anyTotalCount = 1000000, anyNullCount = 79609},NumericRowSummary {numericMinimum = Just 2.0e-3, numericMaximum = Just 688.099, numericAverage = 201.577058958635})})

real	0m31.661s


$ time (./generator 10000000 | cabal run first)

(LineSummary {countInvalidSyntax = 0},Row_ {sessionId = (AnyRowSummary {anyTotalCount = 10000000, anyNullCount = 0},TextualRowSummary {textualCountShortest = Just (8,10000000), textualCountLongest = Just (8,10000000), textualAverageLength = 8.0}), page = (AnyRowSummary {anyTotalCount = 10000000, anyNullCount = 801393},TextualRowSummary {textualCountShortest = Just (5,4599073), textualCountLongest = Just (7,4599534), textualAverageLength = 6.000050116283911}), latency = (AnyRowSummary {anyTotalCount = 10000000, anyNullCount = 0},NumericRowSummary {numericMinimum = Just 4.0, numericMaximum = Just 1613.0, numericAverage = 77.1452806}), timeOnPage = (AnyRowSummary {anyTotalCount = 10000000, anyNullCount = 801393},NumericRowSummary {numericMinimum = Just 0.0, numericMaximum = Just 674.353, numericAverage = 201.26369754410175})})

real	6m3.282s


$ time (./generator 1000000 | cabal run -- first --count)

1000001

real	0m8.779s


$ time (./generator 10000000 | cabal run -- first --count)

10000000

real	1m14.940s

-}
import Wagon.First  
