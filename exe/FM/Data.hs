module FM.Data
  ( YearData (..),
    loadYears,
    makeAttrStandards,
    groupYearsDataByDivision,
    divisionStandards,
    zscoreAttrs,
    clubPointsZScore,
    minutesInPositions,
    correlationData,
    correlationMeanScores,
    correlationScores,
  )
where

import Control.Category ((>>>))
import Data.Bifunctor (second)
import Data.Foldable (fold, toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import FM.Import.Attrs qualified as Attrs
import FM.Import.League qualified as League
import FM.Import.Shared
import FM.Import.Stats qualified as Stats
import FM.Maths (asZScore, weightedMean, weightedStdev)
import FM.Position qualified as Pos
import Math.Regression.Simple qualified as RS
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))

data YearData = YearData
  { yrLeagues :: M.Map FilePath (M.Map T.Text League.Team),
    yrStats :: M.Map Uid Stats.Player,
    yrAttrs :: M.Map Uid (Attrs.Attrs Int)
  }

loadYear :: FilePath -> String -> IO (Day, YearData)
loadYear root folder = do
  let path = root </> folder
  date :: Day <- iso8601ParseM folder
  yrLeagues <- loadLeagues path
  yrStats <- Stats.readPlayers (path </> "stats.html")
  yrAttrs <- Attrs.readPlayers (path </> "attrs.html")
  pure (date, YearData {..})

loadYears :: FilePath -> IO (M.Map Day YearData)
loadYears root = do
  folders <- listDirectory root
  traverse (loadYear root) folders <&> M.fromList

loadLeagues :: FilePath -> IO (M.Map FilePath (M.Map T.Text League.Team))
loadLeagues root =
  listDirectory root
    >>= (filter (List.isPrefixOf "l") >>> pure)
    >>= traverse go
    >>= (fold >>> pure)
  where
    go :: FilePath -> IO (M.Map FilePath (M.Map T.Text League.Team))
    go path = League.readLeague (root </> path) <&> M.singleton (takeBaseName path)

makeAttrStandards ::
  (Foldable t) =>
  t (Attrs.Attrs Int, Double) ->
  Attrs.Attrs (Double, Double)
makeAttrStandards =
  toList
    >>> NE.nonEmpty
    >>> fromMaybe (error "Empty list passed to makeAttrStandards")
    >>> fmap (\(attrs, w) -> fmap (realToFrac >>> (,w) >>> Seq.singleton) attrs)
    >>> M.unionsWith (<>)
    >>> fmap (toList >>> NE.fromList) -- We know the seqs are not empty
    >>> fmap weightedStdev

groupYearsDataByDivision ::
  M.Map Day YearData ->
  M.Map T.Text (Seq.Seq (Stats.Player, Attrs.Attrs Int))
groupYearsDataByDivision yearsData =
  M.elems yearsData
    & concatMap
      ( \YearData {..} ->
          M.intersectionWith
            (,)
            yrStats
            yrAttrs
            & M.elems
            & fmap
              ( \(stats, attrs) ->
                  (Stats.plDivision stats, Seq.singleton (stats, attrs))
              )
      )
    & M.fromListWith (<>)

divisionStandards ::
  M.Map Day YearData ->
  M.Map T.Text (Attrs.Attrs (Double, Double))
divisionStandards = groupYearsDataByDivision >>> fmap doDivision
  where
    doDivision ::
      Seq.Seq (Stats.Player, Attrs.Attrs Int) ->
      Attrs.Attrs (Double, Double)
    doDivision =
      fmap (\(Stats.Player {plMinutes}, attrs) -> (attrs, realToFrac plMinutes))
        >>> makeAttrStandards

zscoreAttrs ::
  Attrs.Attrs (Double, Double) ->
  Attrs.Attrs Int ->
  Attrs.Attrs Double
zscoreAttrs stds attrs = M.intersectionWith asZScore stds (attrs <&> realToFrac)

clubPointsZScore ::
  M.Map Day YearData ->
  M.Map Day (M.Map T.Text Double)
clubPointsZScore yearDatas =
  M.toList leaguePts
    & fmap
      ( \((date, l), clubPts) ->
          ( date,
            clubPts
              & fmap (realToFrac >>> asZScore (leagueStds M.! l))
          )
      )
    & M.fromListWith (<>)
  where
    leaguePts =
      M.toList yearDatas
        & concatMap
          ( \(date, YearData {..}) ->
              M.toList yrLeagues
                & concatMap
                  ( \(fn, tms) ->
                      M.toList tms
                        & concatMap (\(name, League.Team {lgPoints}) -> [((date, fn), M.singleton name lgPoints)])
                  )
          )
        & M.fromListWith (<>)

    leagueStds =
      leaguePts
        & fmap (M.elems >>> fmap (realToFrac >>> (,1)) >>> Seq.fromList)
        & M.mapKeysWith (<>) snd
        & fmap (toList >>> NE.fromList) -- Should not be empty
        & fmap weightedStdev

minutesInPositions :: Pos.Positions -> Stats.Player -> Double
minutesInPositions poss Stats.Player {plPositions, plMinutes} = k * realToFrac plMinutes
  where
    k =
      realToFrac (S.size (S.intersection poss plPositions))
        / realToFrac (S.size plPositions)

correlationData ::
  FilePath ->
  IO (M.Map T.Text (M.Map T.Text (Attrs.Attrs (RS.Fit RS.V2))))
correlationData root = do
  yearsData <- loadYears root
  let divGroups = groupYearsDataByDivision yearsData

  Pos.positionGroups
    & fmap
      ( second $ \poss ->
          divGroups
            & fmap
              ( \rows ->
                  rows
                    & fmap
                      ( \(stats, attrs) ->
                          let mins = minutesInPositions poss stats
                           in attrs
                                & fmap (\val -> Seq.singleton (realToFrac val, Stats.plRating stats, mins))
                      )
                    & M.unionsWith (<>)
                    & fmap
                      (RS.linearWithWeights id)
              )
      )
    & M.fromList
    & pure

correlationMeanScores ::
  M.Map T.Text (M.Map T.Text (Attrs.Attrs (RS.Fit RS.V2))) ->
  M.Map T.Text (Attrs.Attrs Double)
correlationMeanScores =
  correlationScores
    >>> fmap (fmap (fmap ((,1) >>> Seq.singleton)))
    >>> fmap (M.unionsWith (<>))
    >>> fmap (fmap (toList >>> NE.fromList >>> weightedMean >>> max 0))

correlationScores ::
  M.Map T.Text (M.Map T.Text (Attrs.Attrs (RS.Fit RS.V2))) ->
  M.Map T.Text (M.Map T.Text (Attrs.Attrs Double))
correlationScores = fmap score & fmap & fmap
  where
    score :: RS.Fit RS.V2 -> Double
    score
      ( RS.Fit
          { fitParams = RS.V2 slope _off,
            fitErrors = RS.V2 slopeErr _
          }
        ) = slope / sqrt slopeErr
