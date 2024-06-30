module FM.Position
  ( Row (..),
    Col (..),
    Pos (..),
    Poss,
    positions,
    canPlay,
    showPoss,
    readPoss,
  )
where

import Control.Category ((>>>))
import Data.Foldable (fold, toList)
import Data.List qualified as L
import Data.Set qualified as S
import Data.Text qualified as T
import Text.Read (readEither)

data Row = GK | D | DM | WB | M | AM | ST
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
  [ [Pos D L, Pos D C, Pos D R],
    [Pos WB L, Pos DM C, Pos WB R],
    [Pos M L, Pos M C, Pos M R],
    [Pos AM L, Pos AM C, Pos AM R],
    [Pos ST C]
  ]

type Poss = S.Set Pos

instance Show Pos where
  show Pos {..} = show posRow <> show posCol

canPlay :: Poss -> Pos -> Bool
canPlay = flip S.member

showPoss :: Poss -> T.Text
showPoss =
  toList
    >>> fmap (show >>> T.pack)
    >>> L.intersperse ", "
    >>> fold

readPoss :: forall m. (MonadFail m) => String -> m Poss
readPoss =
  T.pack
    >>> T.splitOn ", "
    >>> traverse go
    >>> fmap fold
  where
    go :: T.Text -> m Poss
    go =
      T.splitOn " " >>> \case
        [rstr] -> S.fromList . fmap (`Pos` C) <$> goRows rstr
        [rstr, cstr] -> do
          rs <- goRows rstr
          cs <- goCols cstr
          pure $ S.fromList $ Pos <$> rs <*> cs
        ps -> fail $ "Unexpected position: " <> show ps

    goRows :: T.Text -> m [Row]
    goRows =
      T.splitOn "/"
        >>> traverse (T.unpack >>> readEither >>> either fail pure)

    goCols :: T.Text -> m [Col]
    goCols str = do
      str' <-
        maybe
          (fail $ "Expected like (LC): " <> T.unpack str)
          pure
          $ T.stripPrefix "(" =<< T.stripSuffix ")" str
      traverse
        (pure >>> readEither >>> either fail pure)
        $ T.unpack str'
