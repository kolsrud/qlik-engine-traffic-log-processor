module Lib
    ( processJson 
    ) where

import Types
import GenericUtilities
import Data.Maybe (fromJust)
import qualified Data.Aeson as A

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as H
       
processJson :: String -> MessageProps -> MessageProps
processJson msg props =
  case readObject msg of
    Nothing -> error ("Unable to read object: " ++ msg)
    Just json -> processJsonObject json props

processJsonObject :: A.Object -> MessageProps -> MessageProps
processJsonObject object props = props {
    m_id             = fmap read $ getJsonProperty object ["id"],
    m_method         = getJsonProperty object ["method"],
    m_handle         = fmap read $ getJsonProperty object ["handle"],
    m_assignedHandle = fmap fst objectInfo,
    m_objectInfo     = fmap snd objectInfo,
    m_isError        = hasJsonProperty object ["error"]
  }
 where
  objectInfo = getObjectInfo object

readObject :: String -> Maybe A.Object
readObject = A.decode . BL.fromStrict . BS.pack

getJsonProperty :: A.Object -> [String] -> Maybe String
getJsonProperty obj path = fmap printProp (getObject path obj)

hasJsonProperty :: A.Object -> [String] -> Bool
hasJsonProperty obj path = maybe False (const True) (getJsonProperty obj path) 

getProp :: String -> A.Object -> Maybe A.Value
getProp prop o = H.lookup (T.pack prop) o

getObject :: [String] -> A.Object -> Maybe A.Value
getObject path o = case path of
  []     -> error "getObject: Empty path"
  (p:ps) -> case (getProp p o, ps) of
    (v, []) -> v
    (Just (A.Object newObj), _) -> getObject ps newObj
    _ -> Nothing
        
printProp :: A.Value -> String
printProp value = BL8.unpack $ A.encode value

getObjectInfo :: A.Object -> Maybe (Int, ObjectInfo)
getObjectInfo o = do
  qReturn <- getObject ["result", "qReturn"] o >>= toObject
  handle  <- getJsonProperty qReturn ["qHandle"]
  id      <- getJsonProperty qReturn ["qGenericId"]
  typ     <- getJsonProperty qReturn ["qType"]
  return (read handle, ObjectInfo id typ (getJsonProperty qReturn ["qGenericType"]))
 where
  toObject :: A.Value -> Maybe A.Object
  toObject (A.Object o) = Just o
  toObject _ = Nothing
