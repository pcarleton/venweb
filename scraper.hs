import qualified Data.ByteString.Char8 as B
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import Text.XML.HXT.Core
import Network.HTTP.Conduit

fromFile :: String -> IO (IOSArrow XmlTree (NTree XNode))
fromFile path = do
  contents <- readFile path
  return $ readString [withParseHTML yes, withWarnings no] contents

type CMap = Map String [String]

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

payments f = f >>> css "div" >>>  hasAttrValue "class" ((==) "paymentpage-subline") >>> getChildren >>> hasAttr "href" >>> getAttrValue "href"

-- foldl (im getConns) (return mapStart) firstLinks

getConns dest = do
    putStrLn $ "Getting :" ++ dest
    page <- fromWeb ("https://venmo.com" ++ dest)
    runX . payments $ page
getConns "" = return []

im :: (String -> IO [String]) -> IO CMap -> String -> IO CMap
im f b a = do
    bc <- b
    if Map.member a bc then return bc else insertLinks f bc a
    


insertLinks :: (String -> IO [String]) -> CMap -> String -> IO CMap
insertLinks f b a = do
    res <- f a
    return $ Map.insert a res b
