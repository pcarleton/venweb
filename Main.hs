{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Text.XML.HXT.Core
import qualified Network.HTTP.Conduit as C
import Data.Tree.NTree.TypeDefs
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as LT
import Data.Char (chr)
import Data.List (isPrefixOf)

import Network.HTTP.Types.Status (Status(..))
import Data.Default (def)
import Network (withSocketsDo)

import GHC.Generics

import qualified Data.Aeson as A

import Web.Scotty
import System.Environment
import Network.Wai.Middleware.Static
import Control.Monad.IO.Class (liftIO)

type Username = String
type FullName = String
type Url = String
type Description = String
type UserTupe = ((Username, FullName), Url)
type TransTupe = (Description, (UserTupe, UserTupe))

data User = User {
    name :: Username,
    fullname :: FullName,
    pic :: Url
    } deriving (Show, Generic)

instance A.ToJSON User

data Transaction = Transaction
     {
     source :: User,
     target :: User,
     desc :: Description
     } deriving (Show, Generic)

instance A.ToJSON Transaction

startswith :: String -> String -> Bool
startswith pre = isPrefixOf pre

myUnpack :: L.ByteString -> String
myUnpack = map (chr . fromEnum) . L.unpack

fetchAndParse :: String -> IO (IOSArrow XmlTree XmlTree)
fetchAndParse uri = do
    text <- C.simpleHttp uri
    return $ readString [withParseHTML yes, withWarnings no] $ myUnpack text

css :: String -> IOSArrow XmlTree XmlTree
css tag = multi (hasName tag)

avatarForClass :: String -> IOSArrow XmlTree Url
avatarForClass cls = deep (hasAttrValue "class" (startswith "paymentpage-avatars"))
    >>> picOfClass cls


picOfClass :: String -> IOSArrow XmlTree Url
picOfClass cls = deep (hasName "img")
    >>> hasAttrValue "class" (startswith cls) >>> getAttrValue "src"


userLink :: Int -> IOSArrow XmlTree (String, String)
userLink n = deep (hasAttrValue "class" (startswith "paymentpage-subline"))
    >>> nthLink n

-- inspired by http://stackoverflow.com/questions/17798417/hxt-select-a-node-by-position-with-hxt-in-haskell
junction :: ArrowList a => a b c -> Int -> a b c
junction a nth = a >>. (take 1 . drop nth)

nthLink :: Int -> IOSArrow XmlTree (String, String)
nthLink n = css "a" `junction` n  >>>  getAttrValue "href" &&& deep getText

description :: IOSArrow XmlTree Description
description = deep (hasAttrValue "class" (startswith "paymentpage-text "))
    >>> deep getText

leftUser :: IOSArrow XmlTree UserTupe
leftUser = userLink 0 &&& avatarForClass "to_user"

rightUser :: IOSArrow XmlTree UserTupe
rightUser = userLink 1 &&& avatarForClass "from_user"

transactionArrow :: IOSArrow XmlTree XmlTree -> IOSArrow XmlTree TransTupe
transactionArrow f = f >>> css "div" >>>
    hasAttrValue "class" (startswith "paymentpage-payment-container")
    >>> description &&&  leftUser &&& rightUser

-- Strips preceding slash from user url.
extractUser :: UserTupe -> User
extractUser ut = User (tail . fst . fst $ ut) (snd . fst $ ut) $ snd ut

extractTransaction :: TransTupe -> Transaction
extractTransaction tup = 
    let 
        desc = fst tup
        userTupes = snd tup
        user1 = extractUser (fst userTupes)
        user2 = extractUser (snd userTupes)
    in Transaction user1 user2 desc

transactionsForUser :: Username -> IO [Transaction]
transactionsForUser uname = do
    pageTree <- fetchAndParse ("https://venmo.com/" ++ uname)
    transactionTupes <- runX . transactionArrow $ pageTree
    return $ map extractTransaction transactionTupes

checkStatus200 st@(Status sc _) rh cj =
    if (200 <= sc && sc < 300) || sc == 404
        then Nothing
        else (C.checkStatus def) st rh cj

-- | Download a HTTP link, returning @Nothing@ on 404 status code
downloadCatch404 :: String
                 -> IO LT.Text
downloadCatch404 url = withSocketsDo $ do
    request <- C.parseUrl url
    let request' = request { C.checkStatus = checkStatus200 }
    res <- C.withManager $ C.httpLbs request'
    let status =  statusCode . C.responseStatus $ res
    -- Return Nothing if status code == 404
    return $ if status == 404
        then "missing"
        else "present"



main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/nodes/:username" $ do
      uname <- param "username"
      transactions <- liftIO $ transactionsForUser uname
      json transactions
    get "/ping/:username" $ do
        uname <- param "username"
        msg <- liftIO $ downloadCatch404 ("https://venmo.com/" ++ uname)
        text msg
    get (regex "/graph/.*") $ file "fdg.html"
    get "/faq" $ file "faq.html"
    get "/" $ file "index.html"
