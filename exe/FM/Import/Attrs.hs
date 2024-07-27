module FM.Import.Attrs (Attrs, readPlayers, readAttrs, attrNames) where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Foldable (toList)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import FM.Import (readTable)
import FM.Import.Shared (Uid (..), readTextCell)
import Text.Read (readMaybe)

type Attrs a = M.Map T.Text a

readPlayers :: FilePath -> IO (M.Map Uid (Attrs Int))
readPlayers =
  readTable
    >=> traverse readPlayer
    >=> (M.fromList >>> pure)

readPlayer :: M.Map T.Text T.Text -> IO (Uid, Attrs Int)
readPlayer table = do
  uid <- readTextCell "UID" table
  attrs <- readAttrs table
  pure (Uid uid, attrs)

readAttrs :: M.Map T.Text T.Text -> IO (M.Map T.Text Int)
readAttrs table =
  traverse readAttr (toList attrNames)
    >>= (M.fromList >>> pure)
  where
    readAttr :: T.Text -> IO (T.Text, Int)
    readAttr name =
      readTextCell name table
        >>= (T.unpack >>> readMaybe >>> maybe failure ((name,) >>> pure))
      where
        failure = fail $ "Could not parse as number: " <> show name

attrNames :: S.Set T.Text
attrNames =
  S.fromList
    [ "Wor",
      "Vis",
      "Thr",
      "Tec",
      "Tea",
      "Tck",
      "Str",
      "Sta",
      "TRO",
      "Ref",
      "Pun",
      "Pos",
      "Pen",
      "Pas",
      "Pac",
      "1v1",
      "OtB",
      "Nat",
      "Mar",
      "L Th",
      "Lon",
      "Ldr",
      "Kic",
      "Jum",
      "Hea",
      "Han",
      "Fre",
      "Fla",
      "Fir",
      "Fin",
      "Ecc",
      "Dri",
      "Det",
      "Dec",
      "Cro",
      "Cor",
      "Cnt",
      "Cmp",
      "Com",
      "Cmd",
      "Bra",
      "Bal",
      "Ant",
      "Agi",
      "Agg",
      "Aer",
      "Acc"
    ]
