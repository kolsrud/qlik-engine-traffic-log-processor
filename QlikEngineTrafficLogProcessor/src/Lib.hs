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
       m_assignedHandle = fmap read $ getJsonProperty object ["result", "qReturn", "qHandle"],
       m_isError        = hasJsonProperty object ["error"]
  }

readObject :: String -> Maybe A.Object
readObject = A.decode . BL.fromStrict . BS.pack

getJsonProperty :: A.Object -> [String] -> Maybe String
getJsonProperty obj path = case path of
  []     -> error "getJsonProperty: Empty path"
  [p]    -> fmap printProp (getProp p obj)
  (p:ps) -> case getProp p obj of
    Just (A.Object newObj) -> getJsonProperty newObj ps
    _ -> Nothing

hasJsonProperty :: A.Object -> [String] -> Bool
hasJsonProperty obj path = maybe False (const True) (getJsonProperty obj path) 

getProp :: String -> A.Object -> Maybe A.Value
getProp prop o = H.lookup (T.pack prop) o

printProp :: A.Value -> String
printProp value = BL8.unpack $ A.encode value

