module FM.Import (readTable) where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T

readTable :: FilePath -> IO [M.Map T.Text T.Text]
readTable path = do
  allTxt <- T.readFile path
  let allLines = T.strip <$> T.lines allTxt
  let (headers, rest) = readHeader allLines
  pure $ readRows headers rest

readCell :: T.Text -> Maybe T.Text
readCell line = do
  line' <- T.stripSuffix "</td>" line <|> T.stripSuffix "</th>" line
  T.stripPrefix "<td>" line' <|> T.stripPrefix "<th>" line'

readHeader :: [T.Text] -> ([T.Text], [T.Text])
readHeader = \case
  [] -> error "Could not readHeader"
  (l : ls)
    | T.strip l == "</tr>" -> ([], ls) -- Why do i have to strip again?
    | Just cell <- readCell l ->
        let (cells, rest) = readHeader ls
         in (cell : cells, rest)
    | otherwise -> readHeader ls

readRows :: [T.Text] -> [T.Text] -> [M.Map T.Text T.Text]
readRows headers =
  readRow headers >>> \case
    Nothing -> []
    Just (row, rest) -> row : readRows headers rest

readRow :: [T.Text] -> [T.Text] -> Maybe (M.Map T.Text T.Text, [T.Text])
readRow headers =
  span (/= "</tr>") >>> \case
    (rowLines, rest) ->
      let cells = mapMaybe readCell rowLines
       in if
            | null cells -> Nothing
            | length cells /= length headers -> error $ "Cell length mismatch: " <> show rowLines
            | otherwise -> Just (M.fromList $ zip headers cells, drop 1 rest)
