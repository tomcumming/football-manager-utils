module Main where

import Data.Foldable (fold, forM_, toList)
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FM.Import (readPlayerAttrsTable)
import FM.Roles (Ability (..), Role (..), advancedForward, roleAbility, weightedPercentile)

main :: IO ()
main = do
  inputTxt <- T.getContents
  xs <- readPlayerAttrsTable inputTxt
  let r = advancedForward
  afs <- traverse (roleAbility r) xs
  let percentiles = [0.25, 0.5, 0.75]
  putStrLn $ makeHeaderRow percentiles r
  forM_ (M.toList afs) $ \(name, ab) ->
    putStrLn $ makePlayerRow percentiles name ab

makeHeaderRow :: [Double] -> Role -> String
makeHeaderRow percentiles Role {..} =
  fold $
    intersperse "\t" $
      "Name"
        : ""
        : (show <$> toList rolePrim)
          <> [""]
          <> (show <$> toList roleSnd)
          <> [""]
          <> (show <$> percentiles)

makePlayerRow :: [Double] -> T.Text -> Ability -> String
makePlayerRow percentiles name ab@Ability {..} =
  fold $
    intersperse "\t" $
      T.unpack name
        : ""
        : (show <$> M.elems abilPrim)
          <> [""]
          <> (show <$> M.elems abilSnd)
          <> [""]
          <> (show . weightedPercentile ab 0.5 <$> percentiles)
