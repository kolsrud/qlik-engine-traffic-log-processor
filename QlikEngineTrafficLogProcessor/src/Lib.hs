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
  let m_json = readObject (drop 4 msg)
   in processJsonObject (fromJust m_json) props

processJsonObject :: A.Object -> MessageProps -> MessageProps
processJsonObject object props = props {
       m_id      = fmap read $ getJsonProperty object "id",
       m_method  = getJsonProperty object "method",
       m_handle  = fmap read $ getJsonProperty object "handle",
       m_isError = maybe False (const True) (getJsonProperty object "error")
  }

readObject :: String -> Maybe A.Object
readObject = A.decode . BL.fromStrict . BS.pack

getJsonProperty :: A.Object -> String -> Maybe String
getJsonProperty obj propId = fmap printProp (getProp propId obj)

getProp :: String -> A.Object -> Maybe A.Value
getProp prop o = H.lookup (T.pack prop) o

printProp :: A.Value -> String
printProp value = BL8.unpack $ A.encode value
