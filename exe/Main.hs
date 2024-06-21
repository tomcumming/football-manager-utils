module Main where

import Data.Foldable (fold, forM_, toList)
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FM.Attrs (PlayerAttrs)
import FM.Import (PlayerAttrsTbl, readPlayerAttrsTable)
import FM.Roles (Ability (..), Role (..), RoleName (..), role, roleAbility, weightedPercentile, weightedMean)

relevantRoles :: [RoleName]
-- relevantRoles = [minBound..]
relevantRoles = [AFa, DLFs, Wa, Ws, CMs, CMd, FBa, FBs, CBd]

main :: IO ()
main = do
  inputTxt <- T.getContents
  pat <- readPlayerAttrsTable inputTxt
  -- let rn = CMs
  -- rows <- makeRoleTable rn [1 / 3, 2 / 3] pat
  -- rows <- makeRolesPercentileTable (1/3) pat
  rows <- makeRolesMeanTable (1/2) pat
  forM_ rows $ \row -> putStrLn $ fold $ intersperse "\t" row

makeRoleTable :: (MonadFail m) => RoleName -> [Double] -> PlayerAttrsTbl -> m [[String]]
makeRoleTable rn percentiles pat = do
  pars <- traverse (roleAbility r) pat
  pure $ makeHeaderRow : (uncurry makePlayerRow <$> M.toList pars)
  where
    r = role rn

    makeHeaderRow :: [String]
    makeHeaderRow =
      "Name"
        : ""
        : (show <$> toList (rolePrim r))
          <> [""]
          <> (show <$> toList (roleSnd r))
          <> [""]
          <> (show <$> percentiles)

    makePlayerRow :: T.Text -> Ability -> [String]
    makePlayerRow name ab@Ability {..} =
      T.unpack name
        : ""
        : (show <$> M.elems abilPrim)
          <> [""]
          <> (show <$> M.elems abilSnd)
          <> [""]
          <> (show . (\p -> weightedPercentile (1/3) p ab) <$> percentiles)

makeRolesPercentileTable :: forall m. (MonadFail m) => Double -> PlayerAttrsTbl -> m [[String]]
makeRolesPercentileTable percentile pat = do
  pabs <- traverse playerPs pat
  pure $ headerRow : (uncurry playerRow <$> M.toList pabs)
  where
    headerRow :: [String]
    headerRow = "Name" : "" : (show <$> relevantRoles)

    playerPs :: PlayerAttrs -> m [Integer]
    playerPs pas = do
      abils <- traverse (\rn -> roleAbility (role rn) pas) relevantRoles
      pure $ weightedPercentile (1 / 3) percentile <$> abils

    playerRow :: T.Text -> [Integer] -> [String]
    playerRow name abils = T.unpack name : "" : (show <$> abils)

makeRolesMeanTable :: forall m. (MonadFail m) => Double -> PlayerAttrsTbl -> m [[String]]
makeRolesMeanTable sndWgt pat = do
  pabs <- traverse playerPs pat
  pure $ headerRow : (uncurry playerRow <$> M.toList pabs)
  where
    headerRow :: [String]
    headerRow = "Name" : "" : (show <$> relevantRoles)

    playerPs :: PlayerAttrs -> m [Double]
    playerPs pas = do
      abils <- traverse (\rn -> roleAbility (role rn) pas) relevantRoles
      pure $ weightedMean sndWgt <$> abils

    playerRow :: T.Text -> [Double] -> [String]
    playerRow name abils = T.unpack name : "" : (show <$> abils)
