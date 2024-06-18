module FM.Attrs (Attr (..), lblAttr, attrLbl, PlayerAttrs) where

import Data.Map qualified as M
import Data.Text (Text)
import Data.Tuple (swap)

type PlayerAttrs = M.Map Attr Integer

data Attr
  = Wor -- ^ Work Rate
  | Vis
  | Thr
  | Tec
  | Tea
  | Tck
  | Str
  | Sta
  | TRO
  | Ref
  | Pun
  | Pos
  | Pen
  | Pas
  | Pac
  | Onev1
  | OtB
  | Nat
  | Mar
  | LTh
  | Lon
  | Ldr
  | Kic
  | Jum
  | Hea
  | Han
  | Fre
  | Fla
  | Fir
  | Fin
  | Ecc
  | Dri
  | Det
  | Dec
  | Cro
  | Cor
  | Cnt
  | Cmp
  | Com
  | Cmd
  | Bra
  | Bal
  | Ant
  | Agi
  | Agg
  | Aer
  | Acc
  deriving (Eq, Ord, Show)

attrLbl :: M.Map Attr Text
attrLbl =
  M.fromList
    [ (Wor, "Wor"),
      (Vis, "Vis"),
      (Thr, "Thr"),
      (Tec, "Tec"),
      (Tea, "Tea"),
      (Tck, "Tck"),
      (Str, "Str"),
      (Sta, "Sta"),
      (TRO, "TRO"),
      (Ref, "Ref"),
      (Pun, "Pun"),
      (Pos, "Pos"),
      (Pen, "Pen"),
      (Pas, "Pas"),
      (Pac, "Pac"),
      (Onev1, "1v1"),
      (OtB, "OtB"),
      (Nat, "Nat"),
      (Mar, "Mar"),
      (LTh, "L Th"),
      (Lon, "Lon"),
      (Ldr, "Ldr"),
      (Kic, "Kic"),
      (Jum, "Jum"),
      (Hea, "Hea"),
      (Han, "Han"),
      (Fre, "Fre"),
      (Fla, "Fla"),
      (Fir, "Fir"),
      (Fin, "Fin"),
      (Ecc, "Ecc"),
      (Dri, "Dri"),
      (Det, "Det"),
      (Dec, "Dec"),
      (Cro, "Cro"),
      (Cor, "Cor"),
      (Cnt, "Cnt"),
      (Cmp, "Cmp"),
      (Com, "Com"),
      (Cmd, "Cmd"),
      (Bra, "Bra"),
      (Bal, "Bal"),
      (Ant, "Ant"),
      (Agi, "Agi"),
      (Agg, "Agg"),
      (Aer, "Aer"),
      (Acc, "Acc")
    ]

lblAttr :: M.Map Text Attr
lblAttr =
  M.fromList
    . fmap swap
    $ M.toList attrLbl
