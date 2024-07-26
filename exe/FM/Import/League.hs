module FM.Import.League (readLeague, Team (..)) where

import Control.Category ((>>>))
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import FM.Import (readTable)
import FM.Import.Shared (readIntCell)

data Team = Team
  { lgPlayed :: Int,
    lgFor :: Int,
    lgAgainst :: Int,
    lgPoints :: Int
  }
  deriving (Show)

readLeague :: FilePath -> IO (M.Map T.Text Team)
readLeague path =
  readTable path
    >>= traverse readRow
    >>= (M.fromList >>> pure)

readRow :: M.Map T.Text T.Text -> IO (T.Text, Team)
readRow table = do
  name <- maybe (fail "Can't find team name") pure $ table M.!? "Team"
  lgPlayed <- readIntCell "Pld" table
  lgFor <- readIntCell "For" table
  lgAgainst <- readIntCell "Ag" table
  lgPoints <- readIntCell "Pts" table
  pure (name, Team {..})
