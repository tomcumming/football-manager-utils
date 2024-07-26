module FM.Import.Shared where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Text.Read (readMaybe)

newtype Uid = Uid {unUid :: T.Text}
  deriving (Eq, Ord)
  deriving (Show) via T.Text

readTextCell :: T.Text -> M.Map T.Text T.Text -> IO T.Text
readTextCell name = (M.!? name) >>> maybe (fail $ "Can't find cell: " <> show name) pure

readIntCell :: T.Text -> M.Map T.Text T.Text -> IO Int
readIntCell cell table =
  table M.!? cell & \case
    Nothing -> fail $ "Missing: " <> show cell
    Just s -> case readMaybe (T.unpack (T.filter (/= ',') s)) of
      Just x -> pure x
      Nothing -> fail $ "Can't read as Int: " <> show s
