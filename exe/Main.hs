module Main where

import Data.Foldable (fold, forM_, toList)
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FM.Attrs (PlayerAttrs)
import FM.Import (PlayerAttrsTbl, readPlayerAttrsTable)
import FM.Roles (Ability (..), Role (..), RoleName (..), role, roleAbility, weightedPercentile)

main :: IO ()
main = do
  inputTxt <- T.getContents
  pat <- readPlayerAttrsTable inputTxt
  -- let rn = CMs
  -- rows <- makeRoleTable rn [1 / 3, 2 / 3] pat
  rows <- makeRolesTable (1/3) pat
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
          <> (show . weightedPercentile ab 0.5 <$> percentiles)

makeRolesTable :: forall m. (MonadFail m) => Double -> PlayerAttrsTbl -> m [[String]]
makeRolesTable percentile pat = do
  pabs <- traverse playerPs pat
  pure $ headerRow : (uncurry playerRow <$> M.toList pabs)
  where
    allRoles = [minBound @RoleName ..]

    headerRow :: [String]
    headerRow = "Name" : "" : (show <$> allRoles)

    playerPs :: PlayerAttrs -> m [Integer]
    playerPs pas = do
      abils <- traverse (\rn -> roleAbility (role rn) pas) allRoles
      pure $ (\ab -> weightedPercentile ab (1 / 3) percentile) <$> abils

    playerRow :: T.Text -> [Integer] -> [String]
    playerRow name abils = T.unpack name : "" : (show <$> abils)
