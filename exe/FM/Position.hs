module FM.Position
  ( Row (..),
    Col (..),
    Pos (..),
    Positions,
    positions,
    positionGroups,
    canPlay,
    showPositions,
    readPositions,
  )
where

import Control.Arrow (second)
import Control.Category ((>>>))
import Data.Foldable (fold, toList)
import Data.Function ((&))
import Data.List qualified as L
import Data.Set qualified as S
import Data.Text qualified as T
import Text.Read (readEither)

data Row = GK | D | DM | M | AM | ST
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Col = L | C | R
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Pos = Pos
  { posRow :: Row,
    posCol :: Col
  }
  deriving (Eq, Ord)

positions :: [[Pos]]
positions =
  [ [Pos GK C],
    [Pos D L, Pos D C, Pos D R],
    [Pos DM L, Pos DM C, Pos DM R],
    [Pos M L, Pos M C, Pos M R],
    [Pos AM L, Pos AM C, Pos AM R],
    [Pos ST C]
  ]

positionGroups :: [(T.Text, Positions)]
positionGroups =
  second S.fromList
    <$> [ ("GK", [Pos GK C]),
          ("DC", [Pos D C]),
          ("DLR", [Pos D L, Pos D R]),
          ("DM", [Pos DM C]),
          ("WB", [Pos DM L, Pos DM R]),
          ("MC", [Pos M C]),
          ("MLR", [Pos M L, Pos M R]),
          ("AMC", [Pos AM C]),
          ("AMLR", [Pos AM L, Pos AM R]),
          ("ST", [Pos ST C])
        ]

type Positions = S.Set Pos

instance Show Pos where
  show Pos {..} = show posRow <> show posCol

canPlay :: Positions -> Pos -> Bool
canPlay = flip S.member

showPositions :: Positions -> T.Text
showPositions =
  toList
    >>> fmap (show >>> T.pack)
    >>> L.intersperse ", "
    >>> fold

readPositions :: forall m. (MonadFail m) => String -> m Positions
readPositions =
  T.pack
    >>> T.splitOn ", "
    >>> traverse go
    >>> fmap fold
  where
    go :: T.Text -> m Positions
    go =
      T.splitOn " " >>> \case
        [rstr] -> S.fromList . fmap (`Pos` C) <$> goRows rstr
        [rstr, cstr] -> do
          rs <- goRows rstr
          cs <- goCols cstr
          pure $ S.fromList $ Pos <$> rs <*> cs
        ps -> fail $ "Unexpected position: " <> show ps

    readRow :: T.Text -> m Row
    readRow = \case
      "WB" -> pure DM
      txt -> T.unpack txt & readEither & either (("readRow" <>) >>> fail) pure

    goRows :: T.Text -> m [Row]
    goRows =
      T.splitOn "/"
        >>> traverse readRow

    goCols :: T.Text -> m [Col]
    goCols str = do
      str' <-
        maybe
          (fail $ "Expected like (LC): " <> T.unpack str)
          pure
          $ T.stripPrefix "(" =<< T.stripSuffix ")" str
      traverse
        (pure >>> readEither >>> either (("goCols" <>) >>> fail) pure)
        $ T.unpack str'
