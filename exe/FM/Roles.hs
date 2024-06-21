module FM.Roles
  ( RoleName (..),
    Role (..),
    Ability (..),
    role,
    roleAbility,
    weightedPercentile,
    weightedMean,
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
  | CMa
  | DLFs
  | FBs
  | FBa
  | IFs
  | Ws
  | Wa
  | WBs
  | WBa
  | BWMd
  | BBMs
  | NNFBd
  | ANCHORd
  | DLPs
  | AMs
  | AMa
  deriving (Eq, Ord, Enum, Bounded, Show)

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

weightedMean ::
  -- | Secondary Weight (0 - 1)
  Double ->
  Ability ->
  Double
weightedMean sndWgt Ability {..} =
  (realToFrac (sum abilPrim) + realToFrac (sum abilSnd) * sndWgt)
    / totalWeight
  where
    totalWeight = realToFrac (M.size abilPrim) + realToFrac (M.size abilSnd) * sndWgt

weightedPercentile ::
  -- | Secondary Weight (0 - 1)
  Double ->
  -- | Percentil (0 - 1)
  Double ->
  Ability ->
  Integer
weightedPercentile sndWgt p Ability {..} = go (p * totalWeight) sortedWeights
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
  FBa ->
    Role
      { rolePrim = S.fromList [Cro, Mar, Tck, Ant, Pos, Tea],
        roleSnd = S.fromList [Dri, Fir, Pas, Tec, Cnt, Dec, OtB, Wor, Agi, Pac, Sta]
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
  Wa ->
    Role
      { rolePrim = S.fromList [Cro, Dri, Tec, Acc, Agg],
        roleSnd = S.fromList [Fir, Pas, Ant, Fla, OtB, Wor, Bal, Pac, Sta]
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
  CMa ->
    Role
      { rolePrim = S.fromList [Fir, Pas, Dec, OtB],
        roleSnd = S.fromList [Lon, Tck, Tec, Ant, Com, Tea, Vis, Wor, Acc, Sta]
      }
  IFs ->
    Role
      { rolePrim = S.fromList [Dri, Fin, Fir, Tec, OtB, Acc, Agi],
        roleSnd = S.fromList [Lon, Pas, Ant, Cmp, Fla, Vis, Wor, Bal, Pac, Sta]
      }
  WBa ->
    Role
      { rolePrim = S.fromList [Cro, Dri, Tck, Tec, OtB, Tea, Wor, Acc, Pac, Sta],
        roleSnd = S.fromList [Fir, Mar, Pas, Ant, Cnt, Dec, Fla, Pos, Agi, Bal]
      }
  WBs ->
    Role
      { rolePrim = S.fromList [Cro, Dri, Mar, Tck, OtB, Tea, Wor, Acc, Sta],
        roleSnd = S.fromList [Fir, Pas, Tec, Ant, Cnt, Dec, Pos, Agi, Bal, Pac]
      }
  BWMd ->
    Role
      { rolePrim = S.fromList [Tck, Agg, Ant, Tea, Wor, Sta],
        roleSnd = S.fromList [Mar, Bra, Cnt, Pos, Agi, Pac, Str]
      }
  BBMs ->
    Role
      { rolePrim = S.fromList [Pas, Tck, OtB, Tea, Wor, Sta],
        roleSnd = S.fromList [Dri, Fin, Fir, Lon, Tec, Agg, Ant, Com, Dec, Pos, Acc, Bal, Pac, Str]
      }
  NNFBd ->
    Role
      { rolePrim = S.fromList [Mar, Tck, Ant, Pos, Str],
        roleSnd = S.fromList [Hea, Agg, Bra, Cnt, Tea]
      }
  ANCHORd ->
    Role
      { rolePrim = S.fromList [Mar, Tck, Ant, Cnt, Dec, Pos],
        roleSnd = S.fromList [Com, Tea, Str]
      }
  DLPs ->
    Role
      { rolePrim = S.fromList [Fir, Pas, Tec, Com, Dec, Tea, Vis],
        roleSnd = S.fromList [Ant, OtB, Pos, Bal]
      }
  AMs ->
    Role
      { rolePrim = S.fromList [Fir, Lon, Pas, Ant, Dec, Fla, OtB],
        roleSnd = S.fromList [Dri, Com, Vis, Agi]
      }
  AMa ->
    Role
      { rolePrim = S.fromList [Dri, Fir, Lon, Pas, Tec, Ant, Dec, Fla, OtB],
        roleSnd = S.fromList [Fin, Cmp, Vis, Agi]
      }
