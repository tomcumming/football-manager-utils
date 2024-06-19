module FM.Roles
  ( RoleName (..),
    Role (..),
    Ability (..),
    role,
    roleAbility,
    weightedPercentile,
  )
where

import Control.Arrow ((>>>))
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Set qualified as S
import FM.Attrs (Attr (..), PlayerAttrs)

data RoleName
  = AFa
  | CBd
  | CMd
  | CMs
  | DLFs
  | FBs
  | Ws
  deriving (Eq, Ord, Show)

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

role :: RoleName -> Role
role = \case
  AFa ->
    Role
      { rolePrim = S.fromList [Dri, Fin, Fir, Tec, Cmp, OtB, Acc],
        roleSnd = S.fromList [Pas, Ant, Dec, Wor, Agi, Bal, Pac, Sta]
      }
  DLFs ->
    Role
      { rolePrim = S.fromList [Fir, Pas, Tec, Cmp, Dec, OtB, Tea],
        roleSnd = S.fromList [Fin, Ant, Fla, Vis, Bal, Str]
      }
  FBs ->
    Role
      { rolePrim = S.fromList [Mar, Tck, Ant, Cnt, Pos, Tea],
        roleSnd = S.fromList [Cro, Dri, Pas, Tec, Dec, Wor, Pac, Sta]
      }
  CBd ->
    Role
      { rolePrim = S.fromList [Hea, Mar, Tck, Pos, Jum, Str],
        roleSnd = S.fromList [Agg, Ant, Bra, Cmp, Cnt, Dec, Pac]
      }
  Ws ->
    Role
      { rolePrim = S.fromList [Cro, Dri, Tec, Acc, Agg],
        roleSnd = S.fromList [Fir, Pas, OtB, Wor, Bal, Pac, Sta]
      }
  CMd ->
    Role
      { rolePrim = S.fromList [Tck, Cnt, Dec, Pos, Tea],
        roleSnd = S.fromList [Fir, Mar, Pas, Tec, Agg, Ant, Cmp, Wor, Sta]
      }
  CMs ->
    Role
      { rolePrim = S.fromList [Fir, Pas, Tck, Dec, Tea],
        roleSnd = S.fromList [Tec, Ant, Cmp, Cnt, OtB, Vis, Wor, Sta]
      }
