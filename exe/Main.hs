module Main where

import Control.Monad (guard)
import Data.Foldable (fold, forM_, toList)
import Data.List (intersperse)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FM.Attrs (PlayerAttrs)
import FM.Import (PlayerAttrsTbl, readPlayerAttrsTable)
import FM.Position qualified as P
import FM.Roles (Ability (..), Role (..), RoleName, positionRoles, role, roleAbility, weightedMean, weightedPercentile)
import FM.Roles qualified as RN

relevantRoles :: [RoleName]
relevantRoles = [minBound ..]

-- relevantRoles =
--   [ RN.DFBs
--   , RN.DCDd
--   , RN.MCMd
--   , RN.MCMs
--   , RN.MWs
--   , RN.ADLFs
--   , RN.AAFa
--   ]

main :: IO ()
main = do
  inputTxt <- T.getContents
  pat <- readPlayerAttrsTable inputTxt
  let rn = RN.DCDd
  _rows <- makeRoleTable (P.Pos P.D P.C) rn [1 / 3, 2 / 3] pat
  _rows <- makeRolesPercentileTable (1 / 2) pat
  rows <- makeRolesMeanTable (1 / 2) pat
  _rows <- makePositionsTable (1 / 2) pat
  forM_ rows $ \row -> putStrLn $ fold $ intersperse "\t" row

makeRoleTable ::
  forall m.
  (MonadFail m) =>
  P.Pos ->
  RoleName ->
  [Double] ->
  PlayerAttrsTbl ->
  m [[String]]
makeRoleTable pos rn percentiles pat = do
  pars <- traverse (uncurry foo) pat
  pure $ makeHeaderRow : (uncurry makePlayerRow <$> M.toList pars)
  where
    r = role rn

    foo :: P.Poss -> PlayerAttrs -> m (Maybe Ability)
    foo poss pats
      | poss `P.canPlay` pos = Just <$> roleAbility r pats
      | otherwise = pure Nothing

    makeHeaderRow :: [String]
    makeHeaderRow =
      "Name"
        : ""
        : (show <$> toList (rolePrim r))
          <> [""]
          <> (show <$> toList (roleSnd r))
          <> [""]
          <> (show <$> percentiles)

    makePlayerRow :: T.Text -> Maybe Ability -> [String]
    makePlayerRow name = \case
      Nothing ->
        T.unpack name
          : ""
          : ("" <$ toList (rolePrim r))
            <> [""]
            <> ("" <$ toList (roleSnd r))
            <> [""]
            <> ("" <$ percentiles)
      Just ab@Ability {..} ->
        T.unpack name
          : ""
          : (show <$> M.elems abilPrim)
            <> [""]
            <> (show <$> M.elems abilSnd)
            <> [""]
            <> (show . (\p -> weightedPercentile (1 / 3) p ab) <$> percentiles)

makeRolesPercentileTable :: forall m. (MonadFail m) => Double -> PlayerAttrsTbl -> m [[String]]
makeRolesPercentileTable percentile =
  makeRolesTable (realToFrac . weightedPercentile (1 / 3) percentile)

makeRolesMeanTable :: forall m. (MonadFail m) => Double -> PlayerAttrsTbl -> m [[String]]
makeRolesMeanTable sndWgt = makeRolesTable (weightedMean sndWgt)

makeRolesTable :: forall m. (MonadFail m) => (Ability -> Double) -> PlayerAttrsTbl -> m [[String]]
makeRolesTable calcRoleAbil pat = do
  pabs <- traverse playerPs pat
  pure $ headerRow : (uncurry playerRow <$> M.toList pabs)
  where
    headerRow :: [String]
    headerRow = "Name" : "" : (show <$> relevantRoles)

    playerPs :: (P.Poss, PlayerAttrs) -> m [Maybe Double]
    playerPs (poss, pas) = traverse (playerRoleAbility poss pas) relevantRoles

    playerRoleAbility :: P.Poss -> PlayerAttrs -> RoleName -> m (Maybe Double)
    playerRoleAbility poss pas rn = do
      let playerRoles = foldMap (fromMaybe mempty . (`M.lookup` positionRoles)) poss
      let r = role rn
      ab <- roleAbility r pas
      pure $ do
        guard $ rn `S.member` playerRoles
        Just $ calcRoleAbil ab

    playerRow :: T.Text -> [Maybe Double] -> [String]
    playerRow name abils = T.unpack name : "" : (maybe "" show <$> abils)

makePositionsTable :: forall m. (MonadFail m) => Double -> PlayerAttrsTbl -> m [[String]]
makePositionsTable sndWgt pat = do
  pabs <- traverse playerPs pat
  pure $ headerRow : (uncurry playerRow <$> M.toList pabs)
  where
    headerRow :: [String]
    headerRow = "Name" : "" : fold (L.intersperse [""] (fmap show <$> P.positions))

    playerRow :: T.Text -> [[Maybe Double]] -> [String]
    playerRow name abils = T.unpack name : "" : fold (L.intersperse [""] (fmap (maybe "" show) <$> abils))

    playerPs :: (P.Poss, PlayerAttrs) -> m [[Maybe Double]]
    playerPs (poss, pas) = traverse (traverse (playerPositionAbility poss pas)) P.positions

    playerPositionAbility :: P.Poss -> PlayerAttrs -> P.Pos -> m (Maybe Double)
    playerPositionAbility poss pas pos = do
      let rns = positionRoles M.! pos -- safe...
      let rs = role <$> S.toList rns
      pabs <- traverse (`roleAbility` pas) rs
      let scores = weightedMean sndWgt <$> pabs
      pure $ do
        guard $ poss `P.canPlay` pos
        Just $ maximum scores
