module FM.Roles
  ( advancedForward,
    Role (..),
    Ability (..),
    roleAbility,
    weightedPercentile,
  )
where

import Control.Arrow ((>>>))
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Set qualified as S
import FM.Attrs (Attr (..), PlayerAttrs)

data Role = Role
  { rolePrim :: S.Set Attr,
    roleSnd :: S.Set Attr
  }
  deriving (Eq, Ord, Show)

data Ability = Ability
  { abilPrim :: M.Map Attr Integer,
    abilSnd :: M.Map Attr Integer
  }
  deriving (Eq, Ord, Show)

roleAbility :: (MonadFail m) => Role -> PlayerAttrs -> m Ability
roleAbility Role {..} attrs = do
  abilPrim <- go rolePrim
  abilSnd <- go roleSnd
  pure $ Ability {..}
  where
    go s = fmap M.fromList $ traverse go' $ S.toList s
    go' a =
      maybe
        (fail $ "Can't find " <> show a)
        ((a,) >>> pure)
        $ attrs M.!? a

weightedPercentile ::
  Ability ->
  -- | Secondary Weight (0 - 1)
  Double ->
  -- | Percentil (0 - 1)
  Double ->
  Integer
weightedPercentile Ability {..} sndWgt p = go (p * totalWeight) sortedWeights
  where
    sortedWeights =
      sortOn snd $
        ((1,) <$> M.elems abilPrim) <> ((sndWgt,) <$> M.elems abilSnd)
    totalWeight = sum $ fst <$> sortedWeights

    go remaining = \case
      [] -> error "weightedPercentile []"
      (w, _) : x : xs | w < remaining -> go (remaining - w) (x : xs)
      (_, score) : _ -> score

advancedForward :: Role
advancedForward =
  Role
    { rolePrim = S.fromList [Dri, Fin, Fir, Tec, Cmp, OtB, Acc],
      roleSnd = S.fromList [Pas, Ant, Dec, Wor, Agi, Bal, Pac, Sta]
    }
