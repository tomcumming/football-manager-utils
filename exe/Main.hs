module Main where

import Control.Category ((>>>))
import Data.Bifunctor (second)
import Data.Foldable (fold, forM_, toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import FM.Import.Attrs qualified as Attrs
import FM.Import.League qualified as League
import FM.Import.Shared (Uid)
import FM.Import.Stats qualified as Stats
import FM.Maths (weightedMean, weightedStdev)
import FM.Position qualified as Pos
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, (</>))

main :: IO ()
main =
  getArgs >>= \case
    ["league-relative", root] -> printRelativeAttrsReport root
    ["important", root] -> printImportantAttrs root
    ["test-individual", root, posg] -> testIndividualFit root (T.pack posg)
    ["test-team", root, posg] -> testTeamFit root (T.pack posg)
    args -> putStrLn $ "Unexpected args: " <> show args

-- | Scores attributes transformed to league stdev space
scoreAttributes :: T.Text -> Attrs.Attrs Double -> Double
scoreAttributes posg attrs =
  attrs
    & M.mapWithKey (\k x -> x * if k `S.member` imps then 2 else 1)
    & sum
  where
    imps = importantAttrs M.! posg

testIndividualFit :: FilePath -> T.Text -> IO ()
testIndividualFit root posg = do
  folders <- listDirectory root
  yearData <- traverse (loadYear root) folders <&> M.fromList
  let leagueStds = leagueStandardAttrs yearData

  let teamPoints = foldMap makeRelativePoints . yrLeagues <$> yearData

  posgPoss <-
    List.find (fst >>> (== posg)) Pos.positionGroups
      & maybe (fail $ "Unknown pos group: " <> show posg) pure
      & fmap snd

  putStrLn "Date\tClub\tPoints\tScore"

  forM_ (M.toList yearData) $ \(date, YearData {..}) -> do
    let inPos =
          yrStats
            <&> minutesInPositions posgPoss
            & M.filter (fst >>> (> 0))
    forM_ (M.toList inPos) $ \(uid, (_w, Stats.Player {..})) -> do
      let attrs = leagueRelativeAttributes leagueStds plDivision (yrAttrs M.! uid)
      let pts = (teamPoints M.! date) M.! plClub
      let score = scoreAttributes posg attrs
      putStrLn $
        fold
          [ show date,
            "\t",
            T.unpack plClub,
            "\t",
            show pts,
            "\t",
            show score
          ]

testTeamFit :: FilePath -> T.Text -> IO ()
testTeamFit root posg = do
  folders <- listDirectory root
  yearData <- traverse (loadYear root) folders <&> M.fromList
  let leagueStds = leagueStandardAttrs yearData
  let teamPoints = foldMap makeRelativePoints . yrLeagues <$> yearData

  posgPoss <-
    List.find (fst >>> (== posg)) Pos.positionGroups
      & maybe (fail $ "Unknown pos group: " <> show posg) pure
      & fmap snd

  putStrLn "Date\tClub\tPoints\tScore"

  forM_ (M.toList yearData) $ \(date, YearData {..}) -> do
    let inPos =
          yrStats
            <&> minutesInPositions posgPoss
            & M.filter (fst >>> (> 0))

    let teamAttrs =
          M.toList inPos
            <&> ( \(uid, (w, Stats.Player {..})) ->
                    let attrs = leagueRelativeAttributes leagueStds plDivision (yrAttrs M.! uid)
                     in M.singleton plClub $
                          Seq.singleton
                            ( scoreAttributes posg attrs,
                              w
                            )
                )
            & M.unionsWith (<>)
            & fmap (toList >>> NE.fromList >>> weightedMean)

    forM_ (M.toList teamAttrs) $ \(name, meanSum) -> do
      let pts = (teamPoints M.! date) M.! name

      putStrLn $
        fold
          [ show date,
            "\t",
            T.unpack name,
            "\t",
            show pts,
            "\t",
            show meanSum
          ]

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

loadLeagues :: FilePath -> IO (M.Map FilePath (M.Map T.Text League.Team))
loadLeagues root =
  listDirectory root
    >>= (filter (List.isPrefixOf "l") >>> pure)
    >>= traverse go
    >>= (fold >>> pure)
  where
    go :: FilePath -> IO (M.Map FilePath (M.Map T.Text League.Team))
    go path = League.readLeague (root </> path) <&> M.singleton (takeBaseName path)

minutesInPositions :: Pos.Positions -> Stats.Player -> (Double, Stats.Player)
minutesInPositions poss pl@Stats.Player {plPositions, plMinutes} = (k * realToFrac plMinutes, pl)
  where
    k =
      realToFrac (S.size (S.intersection poss plPositions))
        / realToFrac (S.size plPositions)

-- | Convert divisions into points relative top of the table
makeRelativePoints :: forall f. (Functor f, Foldable f) => f League.Team -> f Double
makeRelativePoints teams =
  (/ realToFrac top)
    . realToFrac
    . League.lgPoints
    <$> teams
  where
    top = maximum $ League.lgPoints <$> teams

leagueStandardAttrs ::
  (Foldable t) =>
  t YearData ->
  M.Map T.Text (Attrs.Attrs (Double, Double))
leagueStandardAttrs =
  toList
    >>> fmap doYear
    >>> M.unionsWith (M.unionWith (<>))
    >>> fmap (fmap (toList >>> NE.fromList >>> weightedStdev))
  where
    doYear :: YearData -> M.Map T.Text (Attrs.Attrs (Seq.Seq (Double, Double)))
    doYear YearData {yrAttrs, yrStats} =
      M.toList yrAttrs
        & fmap
          ( \(uid, attrs) ->
              let Stats.Player {plMinutes, plDivision} = yrStats M.! uid
               in M.singleton
                    plDivision
                    $ fmap (realToFrac >>> (,realToFrac plMinutes) >>> Seq.singleton) attrs
          )
        & M.unionsWith (M.unionWith (<>))

leagueRelativeAttributes ::
  M.Map T.Text (Attrs.Attrs (Double, Double)) ->
  T.Text ->
  Attrs.Attrs Int ->
  Attrs.Attrs Double
leagueRelativeAttributes divStds division =
  M.mapWithKey
    ( \a i ->
        let x = realToFrac i
            (m, d) = divStd M.! a
         in (x - m) / d
    )
  where
    divStd = divStds M.! division

relativeAttrsData ::
  FilePath ->
  IO [(T.Text, M.Map T.Text (Attrs.Attrs (Double, Double)))]
relativeAttrsData root = do
  folders <- listDirectory root
  yearsData <- traverse (loadYear root) folders <&> M.fromList
  let leagueStds = leagueStandardAttrs yearsData

  let doPositionGroup poss =
        toList yearsData
          & fmap (\YearData {yrStats, yrAttrs} -> M.intersectionWith (,) yrStats yrAttrs)
          & foldMap (toList >>> Seq.fromList)
          & fmap
            ( \(pl, attrs) ->
                M.singleton
                  (Stats.plDivision pl)
                  $ Seq.singleton
                    ( minutesInPositions poss pl & fst,
                      leagueRelativeAttributes leagueStds (Stats.plDivision pl) attrs
                    )
            )
          & M.unionsWith (<>)
          & fmap
            ( fmap (\(w, attrs) -> fmap ((,w) >>> Seq.singleton) attrs)
                >>> M.unionsWith (<>)
                >>> fmap (toList >>> NE.fromList >>> weightedStdev)
            )

  Pos.positionGroups & fmap (second doPositionGroup) & pure

printRelativeAttrsReport :: FilePath -> IO ()
printRelativeAttrsReport root = do
  posGroupsData <- relativeAttrsData root

  putStr "Division"
  forM_ Attrs.attrNames $ \name -> putStr ("\t" <> T.unpack name)
  putStrLn ""

  forM_ posGroupsData $ \(posName, divs) -> do
    putStrLn $ T.unpack posName
    forM_ (M.toList divs) $ \(division, attrs) -> do
      putStr $ T.unpack division
      forM_ Attrs.attrNames $ \n -> putStr ("\t" <> show (attrs M.! n & fst))
      putStrLn ""

printImportantAttrs :: FilePath -> IO ()
printImportantAttrs root = do
  posGroupsData <- relativeAttrsData root
  let summary =
        posGroupsData
          & fmap
            ( second
                ( fmap (fmap (fst >>> Seq.singleton))
                    >>> M.unionsWith (<>)
                    >>> fmap (\xs -> sum xs / realToFrac (length xs))
                )
            )
  let topAttrs (m :: M.Map T.Text Double) =
        M.toList m
          & List.sortOn (snd >>> negate)
          & take 11
          & fmap fst
  let important = summary & fmap (second topAttrs)
  forM_ important $ \(name, attrs) -> do
    putStr (T.unpack name)
    forM_ attrs $ \attr -> putStr ("\t" <> T.unpack attr)
    putStrLn ""

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
