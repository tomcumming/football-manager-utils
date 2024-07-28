module Main (main) where

import Control.Category ((>>>))
import Data.Bifunctor (second)
import Data.Foldable (fold, forM_, toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Number.Erf (erf)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (Day)
import FM.Data (YearData (..), clubPointsZScore, correlationData, correlationMeanScores, correlationScores, divisionStandards, groupYearsDataByDivision, loadYears, makeAttrStandards, minutesInPositions, zscoreAttrs)
import FM.Import (readTable)
import FM.Import.Attrs qualified as Attrs
import FM.Import.Stats qualified as Stats
import FM.Maths (asZScore, weightedMean, weightedStdev)
import FM.Position qualified as Pos
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    ["league-relative", root] -> printRelativeAttrsReport root
    ["important", root] -> printImportantAttrs root
    ["test-individual", root, posg] -> testIndividualFit root (T.pack posg)
    ["test-team", root, posg] -> testTeamFit root (T.pack posg)
    ["correlation", root] -> printCorrelationReport root
    "players" : root : player : leagueNames -> printPlayersReport root player (leagueNames <&> T.pack)
    args -> putStrLn $ "Unexpected args: " <> show args

-- | Scores attributes transformed to league stdev space
scoreAttributes :: T.Text -> Attrs.Attrs Double -> Double
scoreAttributes posg attrs =
  attrs
    & M.mapWithKey (\k x -> x * if k `S.member` imps then 2 else 1)
    & sum
  where
    imps = importantAttrs M.! posg

scoreCorrelation :: M.Map T.Text (Attrs.Attrs Double) -> T.Text -> Attrs.Attrs Double -> Double
scoreCorrelation corr posg attrs =
  M.intersectionWith
    (*)
    (corr M.! posg)
    attrs
    & sum

testIndividualFit :: FilePath -> T.Text -> IO ()
testIndividualFit root posg = do
  corr <- correlationData root <&> correlationMeanScores

  yearsData <- loadYears root
  let leagueStds = divisionStandards yearsData
  -- let teamPoints = clubPointsZScore yearsData
  let rows =
        M.toList yearsData
          & concatMap
            ( \(date, YearData {..}) ->
                M.intersectionWith
                  (,)
                  yrStats
                  yrAttrs
                  & M.elems
                  & fmap (date,)
            )

  posgPoss <-
    List.find (fst >>> (== posg)) Pos.positionGroups
      & maybe (fail $ "Unknown pos group: " <> show posg) pure
      & fmap snd

  let inPos =
        rows
          & fmap
            ( \(date, (stats, attrs)) ->
                ((date, minutesInPositions posgPoss stats), (stats, attrs))
            )
          & filter (\((_, mins), _) -> mins > 0)

  putStrLn "Date\tClub\tPoints\tScore"

  forM_ inPos $ \((date, _mins), (stats, attrs)) -> do
    let divStd = leagueStds M.! Stats.plDivision stats
    let rattrs = zscoreAttrs divStd attrs
    -- let score = scoreAttributes posg rattrs
    let score = scoreCorrelation corr posg rattrs
    -- let pts = (teamPoints M.! date) M.! Stats.plClub stats
    let pts = Stats.plRating stats
    putStrLn $
      fold $
        List.intersperse
          "\t"
          [show date, T.unpack $ Stats.plClub stats, show pts, show score]

testTeamFit :: FilePath -> T.Text -> IO ()
testTeamFit root posg = do
  yearsData <- loadYears root
  let leagueStds = divisionStandards yearsData
  let teamPoints = clubPointsZScore yearsData
  let rows =
        M.toList yearsData
          & concatMap
            ( \(date, YearData {..}) ->
                M.intersectionWith
                  (,)
                  yrStats
                  yrAttrs
                  & M.elems
                  & fmap (date,)
            )

  posgPoss <-
    List.find (fst >>> (== posg)) Pos.positionGroups
      & maybe (fail $ "Unknown pos group: " <> show posg) pure
      & fmap snd

  let inPos =
        rows
          & fmap
            ( \(date, (stats, attrs)) ->
                ((date, minutesInPositions posgPoss stats), (stats, attrs))
            )
          & filter (\((_, mins), _) -> mins > 0)
          & fmap
            ( \((date, mins), (stats, attrs)) ->
                ((date, (Stats.plDivision stats, Stats.plClub stats)), Seq.singleton ((mins, stats), attrs))
            )
          & M.fromListWith (<>)

  putStrLn "Date\tClub\tPoints\tScore"

  forM_ (M.toList inPos) $ \((date, (division, club)), clubRows) -> do
    let divStd = leagueStds M.! division

    let maybeScore =
          clubRows
            & fmap
              ( \((mins, _stats), attrs) ->
                  let rattrs = zscoreAttrs divStd attrs
                   in (scoreAttributes posg rattrs, mins)
              )
            & toList
            & NE.nonEmpty
            & fmap weightedMean

    let pts = (teamPoints M.! date) M.! club

    case maybeScore of
      Nothing -> pure ()
      Just score ->
        putStrLn $
          fold $
            List.intersperse
              "\t"
              [show date, T.unpack club, show pts, show score]

-- | Differences between positions
--   Returns: League -> PosGroup -> ...
leaguePositionDifferences ::
  M.Map T.Text (Attrs.Attrs (Double, Double)) ->
  M.Map Day YearData ->
  M.Map T.Text (M.Map T.Text (Attrs.Attrs (Double, Double)))
leaguePositionDifferences lgeStds = groupYearsDataByDivision >>> fmap doDivision
  where
    doDivision ::
      Seq.Seq (Stats.Player, Attrs.Attrs Int) ->
      M.Map T.Text (Attrs.Attrs (Double, Double))
    doDivision divData = M.fromList Pos.positionGroups <&> doPosition divData

    doPosition ::
      Seq.Seq (Stats.Player, Attrs.Attrs Int) ->
      Pos.Positions ->
      Attrs.Attrs (Double, Double)
    doPosition divData poss =
      divData
        & fmap
          ( \(stats, attrs) ->
              zscoreAttrs (lgeStds M.! Stats.plDivision stats) attrs
                <&> (,minutesInPositions poss stats)
          )
        & fmap (fmap Seq.singleton)
        & M.unionsWith (<>)
        & fmap (toList >>> NE.nonEmpty >>> fromMaybe (error "nonEmpty leaguePositionStandards"))
        & fmap weightedStdev

printRelativeAttrsReport :: FilePath -> IO ()
printRelativeAttrsReport root = do
  yearsData <- loadYears root
  let lgeStds = divisionStandards yearsData
  let posDiffs = leaguePositionDifferences lgeStds yearsData

  putStr "Division"
  forM_ Attrs.attrNames $ \name -> putStr ("\t" <> T.unpack name)
  putStrLn ""

  forM_ Pos.positionGroups $ \(posName, _) -> do
    putStrLn $ T.unpack posName
    let divs = fmap (M.! posName) posDiffs
    forM_ (M.toList divs) $ \(division, attrs) -> do
      putStr $ T.unpack division
      forM_ Attrs.attrNames $ \n -> putStr ("\t" <> show (attrs M.! n & fst))
      putStrLn ""

printImportantAttrs :: FilePath -> IO ()
printImportantAttrs root = do
  yearsData <- loadYears root
  let lgeStds = divisionStandards yearsData
  let posDiffs = leaguePositionDifferences lgeStds yearsData

  let averagedMeans =
        posDiffs
          & M.elems
          & fmap (fmap (fmap (fst >>> Seq.singleton)))
          & M.unionsWith (M.unionWith (<>))
          & fmap
            ( fmap
                ( fmap (,1)
                    >>> toList
                    >>> NE.nonEmpty
                    >>> fromMaybe (error "nonEmpty printImportAttrs")
                    >>> weightedMean
                )
            )

  let topAttrs (m :: M.Map T.Text Double) =
        M.toList m
          & List.sortOn (snd >>> negate)
          & take 11
          & fmap fst
  let important = averagedMeans <&> topAttrs
  forM_ (Pos.positionGroups <&> fst) $ \name -> do
    let attrs = important M.! name
    putStr (T.unpack name)
    forM_ attrs $ \attr -> putStr ("\t" <> T.unpack attr)
    putStrLn ""

phi :: Double -> Double
phi z = (1 + erf (z / sqrt 2)) / 2

printPlayersReport :: FilePath -> FilePath -> [T.Text] -> IO ()
printPlayersReport root playersPath leagueNames = do
  corr <- correlationData root <&> correlationMeanScores
  yearsData <- loadYears root
  playerTable <- readTable playersPath
  playerData <-
    traverse
      ( \table -> do
          let name = table M.! "Name"
          pos <- table M.! "Position" & (T.unpack >>> Pos.readPositions)
          attrs <- Attrs.readAttrs table
          M.singleton name (pos, attrs) & pure
      )
      playerTable
      <&> M.unions

  let divStds =
        groupYearsDataByDivision yearsData
          & M.filterWithKey (\k _ -> k `elem` leagueNames)
          & fold
          & fmap (\(stats, attrs) -> (attrs, Stats.plMinutes stats & realToFrac))
          & makeAttrStandards

  let divisionPlayers =
        groupYearsDataByDivision yearsData
          & M.filterWithKey (\k _ -> k `elem` leagueNames)
          & fold
          & fmap (second (zscoreAttrs divStds))

  let scoreFn = scoreCorrelation corr

  let posgScores =
        Pos.positionGroups
          & fmap
            ( \(posg, poss) ->
                ( posg,
                  divisionPlayers
                    & fmap (\(stats, attrs) -> (scoreFn posg attrs, minutesInPositions poss stats))
                    & toList
                    & NE.nonEmpty
                    & fromMaybe (error "nonEmpty printPlayersReport")
                    & weightedStdev
                )
            )
          & M.fromList

  putStr "Name\tMax"
  forM_ Pos.positions $ \posg -> do
    putStr "\t"
    forM_ posg $ show >>> ("\t" <>) >>> putStr
  putStrLn ""

  forM_ (M.toList playerData) $ \(name, (poss, attrs)) -> do
    putStr $ T.unpack name

    let posPercentiles =
          Pos.positions
            & ( fmap
                  ( \posss ->
                      fmap
                        ( \pos ->
                            if
                              | S.notMember pos poss -> Nothing
                              | otherwise ->
                                  let posgName =
                                        List.find (snd >>> elem pos) Pos.positionGroups
                                          & maybe (error "Can't lookup position") fst
                                      scoreStds =
                                        (posgScores M.!? posgName)
                                          & fromMaybe (error "Can't lookup pos group stds")

                                      rattrs = zscoreAttrs divStds attrs
                                      score = scoreFn posgName rattrs
                                      zscore = asZScore scoreStds score
                                   in phi zscore & Just
                        )
                        posss
                  )
              )

    let maxPercentile = concatMap catMaybes posPercentiles & maximum
    putStr ("\t" <> show maxPercentile)

    forM_ posPercentiles $ \posss -> do
      putStr "\t"
      forM_ posss $ \case
        Nothing -> putStr "\t"
        Just p -> putStr ("\t" <> show p)
    putStrLn ""

printCorrelationReport :: FilePath -> IO ()
printCorrelationReport root = do
  cdata <- correlationData root
  let corrs = correlationScores cdata
  let means = correlationMeanScores cdata

  forM_ Pos.positionGroups $ \(posg, _poss) -> do
    let pdata = corrs M.! posg

    putStrLn (T.unpack posg)
    ("Division" : (S.toList Attrs.attrNames <&> T.unpack))
      & List.intersperse "\t"
      & fold
      & putStrLn

    let posMeans = means M.! posg

    ("-" : (S.toList Attrs.attrNames & fmap ((posMeans M.!) >>> show)))
      & List.intersperse "\t"
      & fold
      & putStrLn

    forM_ (M.toList pdata) $ \(division, rows) -> do
      ( T.unpack division
          : (S.toList Attrs.attrNames & fmap ((rows M.!) >>> show))
        )
        & List.intersperse "\t"
        & fold
        & putStrLn

-- Generated from 3 years data, perhaps just generate each time we need?
importantAttrs :: M.Map T.Text (S.Set T.Text)
importantAttrs =
  M.fromList
    [ ("GK", ["Ref", "Aer", "Han", "1v1", "Cmd", "Kic", "Com", "Thr", "TRO", "Pun", "Ecc"]),
      ("DC", ["Hea", "Mar", "Tck", "Jum", "Pos", "Str", "Ldr", "Bra", "Cnt", "Agg", "L Th"]),
      ("DLR", ["L Th", "Mar", "Tck", "Cro", "Pos", "Hea", "Pac", "Sta", "Cnt", "Acc", "Bra"]),
      ("DM", ["Tck", "Pas", "Vis", "Mar", "Wor", "Tea", "Lon", "Pos", "Fre", "Sta", "Fir"]),
      ("WB", ["L Th", "Cro", "Tck", "Mar", "Acc", "Pac", "Cor", "Sta", "Dri", "Wor", "Pos"]),
      ("MC", ["Pas", "Vis", "Lon", "Fre", "Cor", "Fir", "Tec", "Wor", "Tea", "Tck", "Cmp"]),
      ("MLR", ["Fla", "Cro", "Dri", "Cor", "Acc", "Pac", "Agi", "Lon", "Fin", "OtB", "Fre"]),
      ("AMC", ["Fla", "Lon", "Fin", "Tec", "Dri", "Vis", "OtB", "Fir", "Cor", "Fre", "Pen"]),
      ("AMLR", ["Fla", "Dri", "Fin", "Acc", "Pac", "Cro", "OtB", "Lon", "Agi", "Pen", "Tec"]),
      ("ST", ["Fin", "Pen", "OtB", "Fla", "Dri", "Lon", "Hea", "Acc", "Pac", "Fir", "Tec"])
    ]
    & fmap S.fromList
