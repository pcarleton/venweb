{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import Text.XML.HXT.Core
import qualified Network.HTTP.Conduit as C
import Data.Char (chr)
import Control.Monad.IO.Class

import Control.Concurrent.ParallelIO (parallel, stopGlobalPool)

import Data.Map (Map)
import qualified Data.Map as Map 

import Web.Scotty
import System.Environment
import Network.Wai.Middleware.Static

import Data.Monoid (mconcat)

import Data.List
import Data.Maybe
import qualified Data.Aeson as A
import qualified Data.Text as T
import GHC.Generics

import Data.Set (Set)
import qualified Data.Set as Set

data Link = Link
    { source :: Int
    , target  :: Int
    } deriving (Show,Generic)
    
instance A.ToJSON Link
instance A.FromJSON Link

data Node = Node
    { name :: T.Text } deriving (Show,Generic)
    
instance A.ToJSON Node
instance A.FromJSON Node

data Graph = Graph
    {
        nodes :: [Node],
        links :: [Link]
    } deriving (Show, Generic)
instance A.ToJSON Graph
instance A.FromJSON Graph

type NL = (Set String, Set (String, String))
type CMap = Map String [String]

myUnpack :: L.ByteString -> String
myUnpack = map (chr . fromEnum) . L.unpack


fromFile :: String -> IO (IOSArrow XmlTree (NTree XNode))
fromFile path = do
  contents <- readFile path
  return $ readString [withParseHTML yes, withWarnings no] contents


fromWeb :: String -> IO (IOSArrow XmlTree (NTree XNode))
fromWeb uri = do
    text <- C.simpleHttp uri
    return $ readString [withParseHTML yes, withWarnings no] $ myUnpack text

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

payments f = f >>> css "div" >>>  hasAttrValue "class" ("paymentpage-subline" ==)
        >>> getChildren >>> hasAttr "href" >>> getAttrValue "href"

friends f = f >>> css "div" >>>  hasAttrValue "class" ("friend-list" ==)
        >>> getChildren >>> getChildren >>> hasAttr "href" >>> getAttrValue "href"


getConns dest = do
    putStrLn $ "Getting :" ++ dest
    page <- fromWeb ("https://venmo.com" ++ dest)
    runX . payments $ page

getConns "" = return []


insertLinks :: (String -> IO [String]) -> CMap -> String -> IO CMap

insertLinks f b a = do
    res <- f a
    return $ Map.insert a res b


im :: CMap -> String -> IO CMap

im b a = if Map.member a b then return b
        else insertLinks getConns b a


keySet :: CMap -> Set String
keySet m = Set.fromList $ Map.keys m

getWeb :: Int -> [String] -> CMap -> IO CMap

getWeb _ [] b = return b
getWeb mx xs b = --return b
        if mx == 0 then return b
        else 
        let linkMapIOs = parallel $ map (im Map.empty) xs
            lookup = flip $ Map.findWithDefault []
        in 
        do
            nms <- linkMapIOs
            newMap <- return $ foldl Map.union b nms
            qs <- return . concat $ map (lookup newMap) xs
            -- ms <- (Map.findWithDefault [] x) =<< nm
            newSet <- return $ Set.toList $ Set.difference (Set.fromList qs) (Set.fromList xs)
            getWeb (mx - 1) newSet newMap


mergeNLs :: NL -> NL -> NL
mergeNLs a b = (Set.union (fst a) (fst b), Set.union (snd a) (snd b))

getNL :: String -> String -> NL
getNL s t = if s == t then (Set.empty, Set.empty) else (Set.singleton t, Set.singleton (s, t))

getNodeLinks :: (String, [String]) -> NL
getNodeLinks sts = 
    let
        nls = map (getNL (fst sts)) (snd sts)
    in foldl mergeNLs (Set.empty, Set.empty) nls

decon :: CMap -> NL
decon m = foldl mergeNLs (Set.empty, Set.empty) $ map getNodeLinks (Map.toList m)


extractGraph :: CMap -> Graph
extractGraph cm =
    let dcd = decon cm
        nodenames = Set.toList (fst dcd)
        nodelist = map (Node . T.pack) $ Set.toList (fst dcd)
        nodeID x = fromMaybe 0 $ elemIndex x nodenames
        linklist = map (\x -> Link (nodeID (fst x)) (nodeID (snd x))) $ Set.toList (snd dcd)
    in Graph nodelist linklist


graphNames :: Graph -> [String]
graphNames g = map (T.unpack . name) (nodes g)

decodeMap :: Graph -> CMap
decodeMap g =
        let names = map (T.unpack . name) (nodes g)
            nameLookup lnk = ((names !! (source lnk)), (names !! (target lnk)))
            lnkTupes = map nameLookup (links g)
            accumulate m tup = Map.insert (fst tup) ((snd tup) : (Map.findWithDefault [] (fst tup) m)) m
        in foldl accumulate Map.empty lnkTupes


expandGraph :: Graph -> IO Graph

expandGraph g =
        let gmap = decodeMap g
            names = graphNames g
            edgeNames = Set.toList $ Set.difference (Set.fromList names) (Map.keysSet gmap)
            mapResult = getWeb 1 edgeNames gmap
        in fmap extractGraph mapResult


graphForUser :: String -> Int -> IO Graph
graphForUser u deg = 
    let result = getWeb deg ["/" ++ u] Map.empty
    in fmap  extractGraph result

expandInput :: ActionM ()
expandInput = do
        g <- jsonData
        graph <- liftIO $ expandGraph g
        json graph

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/nodes/:word" $ do
      beam <- param "word"
      graph <- liftIO $ graphForUser beam 1
      liftIO stopGlobalPool
      json graph
    get "/" $ file "fdg.html"
    post "/expand" expandInput



