module FM.Roles
  ( RoleName (..),
    Role (..),
    Ability (..),
    role,
    roleAbility,
    weightedPercentile,
    weightedMean,
    positionRoles
  )
where

import Control.Arrow ((>>>))
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Set qualified as S
import FM.Attrs (Attr (..), PlayerAttrs)
import qualified FM.Position as P

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

data RoleName
  = DBPDd
  | DBPDs
  | DBPDc
  | DCDd
  | DCDs
  | DCDc
  | DCWBs
  | DCWBa
  | DFBd
  | DFBs
  | DFBa
  | DIFBd
  | DIWBd
  | DIWBs
  | DIWBa
  | DLd
  | DLs
  | DNNCBd
  | DNNCBs
  | DNNCBc
  | DNNFBd
  | DWCBd
  | DWCBs
  | DWCBa
  | DWBd
  | DWBs
  | DWBa
  | MAPs
  | MAPa
  | MAd
  | MAMs
  | MAMa
  | MBWMd
  | MBWMs
  | MBBMs
  | MCs
  | MCMd
  | MCMs
  | MCMa
  | MDLPd
  | MDLPs
  | MDMd
  | MDMs
  | MDWd
  | MDWs
  | MEs
  | MHBd
  | MIFs
  | MIFa
  | MIWs
  | MIWa
  | MMs
  | MMa
  | MRaa
  | MRes
  | MRPMs
  | MSVs
  | MSVa
  | MSSa
  | MWMd
  | MWMs
  | MWMa
  | MWPMs
  | MWPMa
  | MWTFs
  | MWTFa
  | MWs
  | MWa
  | AAFa
  | ACFs
  | ACFa
  | ADLFs
  | ADLFa
  | AFNs
  | APa
  | APFd
  | APFs
  | APFa
  | ATFs
  | ATFa
  | ATa
  deriving (Eq, Ord, Enum, Bounded, Show)

role :: RoleName -> Role
role = \case
  DBPDd ->
    go
      [Hea, Mar, Pas, Tck, Com, Pos, Jum, Str]
      [Fir, Tec, Agg, Ant, Bra, Cnt, Dec, Vis, Pac]
  DBPDs ->
    go
      [Hea, Pas, Tck, Agg, Bra, Com, Dec, Pos, Jum, Str]
      [Fir, Mar, Tec, Ant, Cnt, Vis]
  DBPDc ->
    go
      [Mar, Pas, Tck, Ant, Com, Cnt, Dec, Pos, Pac]
      [Fir, Hea, Tec, Bra, Vis, Jum, Str]
  DCDd ->
    go
      [Hea, Mar, Tck, Pos, Jum, Str]
      [Agg, Ant, Bra, Com, Cnt, Dec, Pas]
  DCDs ->
    go
      [Hea, Tck, Agg, Bra, Dec, Pos, Jum, Str]
      [Mar, Ant, Com, Cnt]
  DCDc ->
    go
      [Mar, Tck, Ant, Cnt, Dec, Pos, Pac]
      [Hea, Bra, Com, Jum, Str]
  DCWBs ->
    go
      [Cro, Dri, Tec, OtB, Tea, Wor, Acc, Sta]
      [Fir, Mar, Pas, Tck, Ant, Dec, Fla, Pos, Agi, Bal, Pac]
  DCWBa ->
    go
      [Cro, Dri, Tec, Fla, OtB, Tea, Wor, Acc, Sta]
      [Fir, Mar, Pas, Tck, Ant, Dec, Pos, Agi, Bal, Pac]
  DFBd ->
    go
      [Mar, Tck, Ant, Cnt, Pos]
      [Cro, Pas, Dec, Tea, Wor, Pac, Sta]
  DFBs ->
    go
      [Mar, Tck, Ant, Cnt, Pos, Tea]
      [Cro, Dri, Pas, Tec, Dec, Wor, Pac, Sta]
  DFBa ->
    go
      [Cro, Mar, Tck, Ant, Pos, Tea]
      [Dri, Fir, Pas, Tec, Cnt, Dec, OtB, Wor, Agi, Pac, Sta]
  DIFBd ->
    go
      [Hea, Mar, Tck, Pos, Str]
      [Dri, Fir, Pas, Tec, Agg, Ant, Bra, Com, Cnt, Dec, Wor, Agi, Jum, Pac]
  DIWBd ->
    go
      [Pas, Tck, Ant, Dec, Pos, Tea]
      [Fir, Mar, Tec, Com, Cnt, OtB, Wor, Acc, Agi, Sta]
  DIWBs ->
    go
      [Fir, Pas, Tck, Com, Dec, Tea]
      [Mar, Tec, Ant, Cnt, OtB, Pos, Vis, Wor, Acc, Agi, Sta]
  DIWBa ->
    go
      [Fir, Pas, Tck, Tec, Com, Dec, OtB, Tea, Vis, Acc]
      [Cro, Dri, Lon, Mar, Ant, Cnt, Fla, Pos, Wor, Agi, Pac, Sta]
  DLd ->
    go
      [Fir, Hea, Mar, Pas, Tck, Tec, Com, Dec, Pos, Tea, Jum, Str]
      [Ant, Bra, Cnt, Pac, Sta]
  DLs ->
    go
      [Fir, Hea, Mar, Pas, Tck, Tec, Com, Dec, Pos, Tea, Jum, Str]
      [Dri, Ant, Bra, Cnt, Vis, Pac, Sta]
  DNNCBd ->
    go
      [Hea, Mar, Tck, Pos, Jum, Str]
      [Agg, Ant, Bra, Cnt, Pac]
  DNNCBs ->
    go
      [Hea, Tck, Agg, Bra, Pos, Jum, Str]
      [Mar, Ant, Cnt]
  DNNCBc ->
    go
      [Mar, Tck, Ant, Cnt, Pos, Pac]
      [Hea, Bra, Jum, Str]
  DNNFBd ->
    go
      [Mar, Tck, Ant, Pos, Str]
      [Hea, Agg, Bra, Cnt, Tea]
  DWCBd ->
    go
      [Hea, Mar, Tck, Pos, Jum, Str]
      [Dri, Fir, Pas, Tec, Agg, Ant, Bra, Com, Cnt, Dec, Wor, Agi, Pac]
  DWCBs ->
    go
      [Dri, Hea, Mar, Tck, Pos, Jum, Pac, Str]
      [Cro, Fir, Pas, Tec, Agg, Ant, Bra, Com, Cnt, Dec, OtB, Wor, Agi, Sta]
  DWCBa ->
    go
      [Cro, Dri, Hea, Mar, Tck, OtB, Jum, Pac, Sta, Str]
      [Fir, Pas, Tec, Agg, Ant, Bra, Com, Cnt, Dec, Pos, Wor, Agi]
  DWBd ->
    go
      [Mar, Tck, Ant, Pos, Tea, Wor, Acc, Sta]
      [Cro, Dri, Fir, Pas, Tec, Cnt, Dec, OtB, Agi, Bal, Pac]
  DWBs ->
    go
      [Cro, Dri, Mar, Tck, OtB, Tea, Wor, Acc, Sta]
      [Fir, Pas, Tec, Ant, Cnt, Dec, Pos, Agi, Bal, Pac]
  DWBa ->
    go
      [Cro, Dri, Tck, Tec, OtB, Tea, Wor, Acc, Pac, Sta]
      [Fir, Mar, Pas, Ant, Cnt, Dec, Fla, Pos, Agi, Bal]
  MAPs ->
    go
      [Fir, Pas, Tec, Com, Dec, OtB, Tea, Vis]
      [Dri, Ant, Fla, Agi]
  MAPa ->
    go
      [Fir, Pas, Tec, Com, Dec, OtB, Tea, Vis]
      [Dri, Ant, Fla, Acc, Agi]
  MAd ->
    go
      [Mar, Tck, Ant, Cnt, Dec, Pos]
      [Com, Tea, Str]
  MAMs ->
    go
      [Fir, Lon, Pas, Tec, Ant, Dec, Fla, OtB]
      [Dri, Com, Vis, Agi]
  MAMa ->
    go
      [Dri, Fir, Lon, Pas, Tec, Ant, Dec, Fla, OtB]
      [Fin, Com, Vis, Agi]
  MBWMd ->
    go
      [Tck, Agg, Ant, Tea, Wor, Sta]
      [Mar, Bra, Cnt, Pos, Agi, Pac, Str]
  MBWMs ->
    go
      [Tck, Agg, Ant, Tea, Wor, Sta]
      [Mar, Pas, Bra, Cnt, Agi, Pac, Str]
  MBBMs ->
    go
      [Pas, Tck, OtB, Tea, Wor, Sta]
      [Dri, Fin, Fir, Lon, Tec, Agg, Ant, Com, Dec, Pos, Acc, Bal, Pac, Str]
  MCs ->
    go
      [Fir, Pas, Tck, Dec, Pos, Tea, Sta]
      [Tec, Ant, Com, Cnt, OtB, Vis, Wor]
  MCMd ->
    go
      [Tck, Cnt, Dec, Pos, Tea]
      [Fir, Mar, Pas, Tec, Agg, Ant, Com, Wor, Sta]
  MCMs ->
    go
      [Fir, Pas, Tck, Dec, Tea]
      [Tec, Ant, Com, Cnt, OtB, Vis, Wor, Sta]
  MCMa ->
    go
      [Fir, Pas, Dec, OtB]
      [Lon, Tck, Tec, Ant, Com, Tea, Vis, Wor, Acc, Sta]
  MDLPd ->
    go
      [Fir, Pas, Tec, Com, Dec, Tea, Vis]
      [Tck, Ant, Pos, Bal]
  MDLPs ->
    go
      [Fir, Pas, Tec, Com, Dec, Tea, Vis]
      [Ant, OtB, Pos, Bal]
  MDMd ->
    go
      [Tck, Ant, Cnt, Pos, Tea]
      [Mar, Pas, Agg, Com, Dec, Wor, Sta, Str]
  MDMs ->
    go
      [Tck, Ant, Cnt, Pos, Tea]
      [Fir, Mar, Pas, Agg, Com, Dec, Wor, Sta, Str]
  MDWd ->
    go
      [Tec, Ant, OtB, Pos, Tea, Wor, Sta]
      [Cro, Dri, Fir, Mar, Tck, Agg, Cnt, Dec, Acc]
  MDWs ->
    go
      [Cro, Tec, OtB, Tea, Wor, Sta]
      [Dri, Fir, Mar, Pas, Tck, Agg, Ant, Com, Cnt, Dec, Pos, Acc]
  MEs ->
    go
      [Fir, Pas, Tec, Com, Dec, Vis]
      [Dri, Ant, Fla, OtB, Tea, Agi]
  MHBd ->
    go
      [Mar, Tck, Ant, Com, Cnt, Dec, Pos, Tea]
      [Fir, Pas, Agg, Bra, Wor, Jum, Sta, Str]
  MIFs ->
    go
      [Dri, Fin, Fir, Tec, OtB, Acc, Agi]
      [Lon, Pas, Ant, Com, Fla, Vis, Wor, Bal, Pac, Sta]
  MIFa ->
    go
      [Dri, Fin, Fir, Tec, Ant, OtB, Acc, Agi]
      [Lon, Pas, Com, Fla, Wor, Bal, Pac, Sta]
  MIWs ->
    go
      [Cro, Dri, Pas, Tec, Acc, Agi]
      [Fir, Lon, Com, Dec, OtB, Vis, Wor, Bal, Pac, Sta]
  MIWa ->
    go
      [Cro, Dri, Pas, Tec, Acc, Agi]
      [Fir, Lon, Ant, Com, Dec, Fla, OtB, Vis, Wor, Bal, Pac, Sta]
  MMs ->
    go
      [Pas, Tec, Dec, OtB, Wor, Acc]
      [Dri, Fir, Lon, Tck, Ant, Com, Vis, Bal, Sta]
  MMa ->
    go
      [Dri, Pas, Tec, Dec, OtB, Vis, Wor, Acc]
      [Fin, Fir, Lon, Ant, Com, Fla, Bal, Sta]
  MRaa ->
    go
      [Fin, Ant, Com, Cnt, Dec, OtB, Bal]
      [Fir, Tec, Wor, Acc, Sta]
  MRes ->
    go
      [Fir, Pas, Tec, Com, Dec, Fla, OtB, Tea, Vis]
      [Dri, Lon, Ant, Bal]
  MRPMs ->
    go
      [Fir, Pas, Tec, Ant, Com, Dec, OtB, Tea, Vis, Wor, Acc, Sta]
      [Dri, Lon, Cnt, Pos, Agi, Bal, Pac]
  MSVs ->
    go
      [Mar, Pas, Tck, OtB, Pos, Wor, Pac, Sta]
      [Fin, Fir, Lon, Ant, Com, Cnt, Dec, Acc, Bal, Str]
  MSVa ->
    go
      [Fin, Lon, Pas, Tck, Ant, OtB, Pos, Wor, Pac, Sta]
      [Fir, Mar, Com, Cnt, Dec, Acc, Bal, Str]
  MSSa ->
    go
      [Dri, Fin, Fir, Ant, Com, OtB, Acc]
      [Pas, Tec, Cnt, Dec, Wor, Agi, Bal, Pac, Sta]
  MWMd ->
    go
      [Pas, Tck, Cnt, Dec, Pos, Tea, Wor]
      [Cro, Fir, Mar, Tec, Ant, Com, Sta]
  MWMs ->
    go
      [Pas, Tck, Dec, Tea, Wor, Sta]
      [Cro, Fir, Tec, Ant, Com, Cnt, OtB, Pos, Vis]
  MWMa ->
    go
      [Cro, Fir, Pas, Dec, Tea, Wor, Sta]
      [Tck, Tec, Ant, Com, OtB, Vis]
  MWPMs ->
    go
      [Fir, Pas, Tec, Com, Dec, Tea, Vis]
      [Dri, OtB, Agi]
  MWPMa ->
    go
      [Dri, Fir, Pas, Tec, Com, Dec, OtB, Tea, Vis]
      [Ant, Fla, Acc, Agi]
  MWTFs ->
    go
      [Hea, Bra, Tea, Jum, Str]
      [Cro, Fir, Ant, OtB, Wor, Bal, Sta]
  MWTFa ->
    go
      [Hea, Bra, OtB, Jum, Str]
      [Cro, Fin, Fir, Ant, Tea, Wor, Bal, Sta]
  MWs ->
    go
      [Cro, Dri, Tec, Acc, Agi]
      [Fir, Pas, OtB, Wor, Bal, Pac, Sta]
  MWa ->
    go
      [Cro, Dri, Tec, Acc, Agi]
      [Fir, Pas, Ant, Fla, OtB, Wor, Bal, Pac, Sta]
  AAFa ->
    go
      [Dri, Fin, Fir, Tec, Com, OtB, Acc]
      [Pas, Ant, Dec, Wor, Agi, Bal, Pac, Sta]
  ACFs ->
    go
      [Dri, Fir, Hea, Lon, Pas, Tec, Ant, Com, Dec, OtB, Vis, Acc, Agi, Str]
      [Fin, Tea, Wor, Bal, Jum, Pac, Sta]
  ACFa ->
    go
      [Dri, Fin, Fir, Hea, Tec, Ant, Com, OtB, Acc, Agi, Str]
      [Lon, Pas, Dec, Tea, Vis, Wor, Bal, Jum, Pac, Sta]
  ADLFs ->
    go
      [Fir, Pas, Tec, Com, Dec, OtB, Tea]
      [Fin, Ant, Fla, Vis, Bal, Str]
  ADLFa ->
    go
      [Fir, Pas, Tec, Com, Dec, OtB, Tea]
      [Dri, Fin, Ant, Fla, Vis, Bal, Str]
  AFNs ->
    go
      [Dri, Fir, Pas, Tec, Com, Dec, OtB, Vis, Acc, Agi]
      [Fin, Ant, Fla, Tea, Bal]
  APa ->
    go
      [Fin, Ant, Com, OtB]
      [Fir, Hea, Tec, Dec, Acc]
  APFd ->
    go
      [Agg, Ant, Bra, Dec, Tea, Wor, Acc, Pac, Sta]
      [Fir, Com, Agi, Bal, Str]
  APFs ->
    go
      [Agg, Ant, Bra, Dec, Tea, Wor, Acc, Pac, Sta]
      [Fir, Pas, Com, Cnt, OtB, Agi, Bal, Str]
  APFa ->
    go
      [Agg, Ant, Bra, OtB, Tea, Wor, Acc, Pac, Sta]
      [Fin, Fir, Com, Cnt, Dec, Agi, Bal, Str]
  ATFs ->
    go
      [Hea, Bra, Tea, Bal, Jum, Str]
      [Fin, Fir, Agg, Ant, Com, Dec, OtB]
  ATFa ->
    go
      [Fin, Hea, Bra, Com, OtB, Bal, Jum, Str]
      [Fir, Agg, Ant, Dec, Tea]
  ATa ->
    go
      [Dri, Fir, Pas, Tec, Com, Dec, Fla, OtB, Vis, Acc]
      [Fin, Ant, Agi, Bal]
  where
    go prims snds = Role (S.fromList prims) (S.fromList snds)

positionRoles :: M.Map P.Pos (S.Set RoleName)
positionRoles = M.fromList
  [ (P.Pos P.D P.L, S.fromList [DFBd, DFBs, DFBa, DWBd, DWBs, DWBa, DNNFBd, DCWBs, DCWBa, DIWBd, DIWBs, DIWBa, DIFBd])
  , (P.Pos P.D P.C, S.fromList [DCDd, DCDs, DCDc, DLd, DLs, DBPDd, DBPDs, DBPDc, DNNCBd, DNNCBs, DNNCBc, DWCBd, DWCBs, DWCBa])
  , (P.Pos P.D P.R, S.fromList [DFBd, DFBs, DFBa, DWBd, DWBs, DWBa, DNNFBd, DCWBs, DCWBa, DIWBd, DIWBs, DIWBa, DIFBd])

  , (P.Pos P.WB P.L, S.fromList [DWBd, DWBs, DWBa, DCWBs, DCWBa, DIWBd, DIWBs, DIWBa])
  , (P.Pos P.DM P.C, S.fromList [MDMd, MDMs, MDLPd, MDLPs, MBWMd, MBWMs, MAd, MHBd, MRes, MRPMs, MSVs, MSVa])
  , (P.Pos P.WB P.R, S.fromList [DWBd, DWBs, DWBa, DCWBs, DCWBa, DIWBd, DIWBs, DIWBa])

  , (P.Pos P.M P.L, S.fromList [MWMd, MWMs, MWMa, MWs, MWa, MDWd, MDWs, MWPMs, MWPMa, MIWs, MIWa])
  , (P.Pos P.M P.C, S.fromList [MCMd, MCMs, MCMa, MDLPd, MDLPs, MBBMs, MAPs, MAPa, MBWMd, MBWMs, MRPMs, MMs, MMa, MCs])
  , (P.Pos P.M P.R, S.fromList [MWMd, MWMs, MWMa, MWs, MWa, MDWd, MDWs, MWPMs, MWPMa, MIWs, MIWa])

  , (P.Pos P.AM P.L, S.fromList [MWs, MWa, MAPs, MAPa, MIFs, MIFa, ATa, MWTFs, MWTFa, MRaa, MIWs, MIWa])
  , (P.Pos P.AM P.C, S.fromList [MAMs, MAMa, MAPs, MAPa, ATa, MEs, MSSa])
  , (P.Pos P.AM P.R, S.fromList [MWs, MWa, MAPs, MAPa, MIFs, MIFa, ATa, MWTFs, MWTFa, MRaa, MIWs, MIWa])

  , (P.Pos P.ST P.C, S.fromList [ADLFs, ADLFa, AAFa, ATFs, ATFa, APa, ACFs, ACFa, APFd, APFs, APFa, ATa, AFNs])

  ]
