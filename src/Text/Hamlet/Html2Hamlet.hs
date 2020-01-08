{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Hamlet.Html2Hamlet
  ( convert2Doc
  , convert2Text
  , convert2TextStrict
  ) where

import           Control.Arrow
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Char
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Text.HTML.DOM                as HTML
import qualified Text.PrettyPrint.Leijen.Text as PP
import           Text.XML
import           Text.XML.Cursor              (child, fromDocument, node)


convert2Text :: L.ByteString -> TL.Text
convert2Text htmlBytes = PP.displayT $ PP.renderPretty 0.4 80 (convert2Doc htmlBytes)

convert2TextStrict :: L.ByteString -> T.Text
convert2TextStrict = TL.toStrict . convert2Text

convert2Doc :: L.ByteString -> PP.Doc
convert2Doc = cvt . fromDocument . HTML.parseLBS where
  cvt doc = "$doctype 5" PP.<$$> go doc
  go cur = fromNode (node cur) PP.<$$> PP.indent 4 (PP.vsep (map go $ child cur))

fromNode :: Node -> PP.Doc
fromNode (NodeElement (Element tag attrs _)) =
  "<" <> PP.hsep (text' (nameLocalName tag): battr attrs) <> ">"
fromNode (NodeContent t    )
  | T.all isSpace t = mempty
  | otherwise       = PP.string $ TL.fromStrict $ T.unlines $ map (\l -> T.concat ["\\ ", l]) $ T.lines $ T.dropWhile isSpace t
fromNode (NodeComment t    ) = PP.vsep $ map (("$# " <>) . text') $ T.lines t
fromNode (NodeInstruction _) = mempty

battr :: Map.Map Name T.Text -> [PP.Doc]
battr = concatMap (f . first nameLocalName) . Map.toList where
  f ("class", val) = map (("." <>) . text') $ T.words val
  f ("id",    val) = ["#" <> text' val]
  f (key,     val) = [text' key <> "=\"" <> text' val <> "\""]

text' :: T.Text -> PP.Doc
text' = PP.text . TL.fromStrict . (T.replace "\"" "&quot;")
