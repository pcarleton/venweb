import qualified Data.ByteString.Char8 as B
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import Text.XML.HXT.Core
import Network.HTTP.Conduit

fromFile :: String -> IO (IOSArrow XmlTree (NTree XNode))
fromFile path = do
  contents <- readFile path
  return $ readString [withParseHTML yes, withWarnings no] contents


css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

payments f = f >>> css "div" >>>  hasAttrValue "class" ((==) "paymentpage-subline") >>> getChildren >>> hasAttr "href" >>> getAttrValue "href"
