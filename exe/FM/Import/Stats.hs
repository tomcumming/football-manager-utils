module FM.Import.Stats (readPlayers, Player (..)) where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Foldable (fold)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import FM.Import (readTable)
import FM.Import.Shared (Uid (..), readIntCell, readTextCell)
import FM.Position (Positions, readPositions)
import Text.Read (readMaybe)

{- Cells
UID
Name
DoB
Division
Club
Rec
Wage
Inf
Position
Apps
Mins
PoM
Av Rat
Gls/90
xA/90
Drb/90
Conv %
Cr C/A
Hdr %
Pas %
Tck R
Shot %
Sv %
-}

data Player = Player
  { plName :: T.Text,
    plDoB :: Day,
    plDivision :: T.Text,
    plClub :: T.Text,
    plPositions :: Positions,
    plMinutes :: Int,
    plRating :: Double,
    plHeaders :: Maybe Double
  }
  deriving (Show)

readPlayers :: FilePath -> IO (M.Map Uid Player)
readPlayers =
  readTable
    >=> traverse readPlayer
    >=> (M.fromList >>> pure)

readPercent :: T.Text -> IO (Maybe Double)
readPercent txt
  | txt == "-" = pure Nothing
  | Just p <- T.stripSuffix "%" txt >>= (T.unpack >>> readMaybe) = pure $ Just p
  | otherwise = fail $ "Failed to read %: " <> show txt

readPlayer :: M.Map T.Text T.Text -> IO (Uid, Player)
readPlayer table = do
  uid <- readTextCell "UID" table
  plName <- readTextCell "Name" table
  plDoB <- parseDoB =<< readTextCell "DoB" table
  plDivision <- readTextCell "Division" table
  plClub <- readTextCell "Club" table
  plPositions <- readTextCell "Position" table >>= (T.unpack >>> readPositions)
  plMinutes <- readIntCell "Mins" table
  plRating <- readTextCell "Av Rat" table 
    >>= (T.unpack >>> readMaybe >>> maybe (fail "Can't read rating") pure)
  plHeaders <- readTextCell "Hdr %" table >>= readPercent
  pure (Uid uid, Player {..})

parseDoB :: T.Text -> IO Day
parseDoB =
  T.splitOn " " >>> \case
    [dateStr, _, _, _]
      | [d, m, y] <- T.splitOn "/" dateStr ->
          iso8601ParseM $ T.unpack $ fold [y, "-", go m, "-", go d]
    xs -> fail $ "Can't parse DoB: " <> show xs
  where
    go s
      | T.length s == 1 = "0" <> s
      | T.length s == 2 = s
      | otherwise = error $ "Invalid day/month: " <> show s
