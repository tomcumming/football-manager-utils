module FM.Import (readPlayerAttrsTable) where

import Control.Category ((>>>))
import Control.Monad (when)
import Data.Map qualified as M
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import FM.Attrs (Attr, PlayerAttrs, lblAttr)
import Text.Read (readMaybe)
import Text.XML.Light qualified as Xml

type PlayerAttrsTbl = M.Map Text PlayerAttrs

readPlayerAttrsTable :: forall m. (MonadFail m) => Text -> m PlayerAttrsTbl
readPlayerAttrsTable =
  Xml.parseXML
    >>> parse
  where
    parse :: (MonadFail m) => [Xml.Content] -> m PlayerAttrsTbl
    parse rootNodes = do
      let attrTbl = M.fromList $ do
            html <- Xml.onlyElems rootNodes
            body <- maybeToList $ Xml.findChild (Xml.unqual "body") html
            wrapperP <- maybeToList $ Xml.findChild (Xml.unqual "p") body
            tbl <- maybeToList $ Xml.findChild (Xml.unqual "table") wrapperP
            (trh : trs) <- pure $ Xml.findChildren (Xml.unqual "tr") tbl
            let hmap = headerMap trh
            tr <- trs
            -- TODO ensure name is first col
            tds@(tdName : _) <- pure $ Xml.elChildren tr
            let name = T.pack $ Xml.strContent tdName
            let attrs =
                  M.fromList $
                    catMaybes $
                      zipWith
                        ( \attr' td -> do
                            attr <- attr'
                            score <- readMaybe $ Xml.strContent td
                            pure (attr, score)
                        )
                        hmap
                        tds
            pure (name, attrs)
      when (M.null attrTbl) $ fail "No entries found in table"
      pure attrTbl

    headerMap :: Xml.Element -> [Maybe Attr]
    headerMap =
      Xml.findChildren (Xml.unqual "th")
        >>> fmap (Xml.strContent >>> T.pack >>> (lblAttr M.!?))
