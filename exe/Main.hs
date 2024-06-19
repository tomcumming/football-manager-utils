module Main where

import Data.Foldable (fold, forM_, toList)
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FM.Import (PlayerAttrsTbl, readPlayerAttrsTable)
import FM.Roles (Ability (..), Role (..), RoleName (..), role, roleAbility, weightedPercentile)

main :: IO ()
main = do
  inputTxt <- T.getContents
  xs <- readPlayerAttrsTable inputTxt
  let rn = AFa
  rows <- makeRoleTable rn [0.25, 0.5, 75] xs
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
