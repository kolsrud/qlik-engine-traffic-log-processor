module Types
    ( MessageProps(..)
    , SessionInfo(..)
    , ObjectInfo(..)
    , defaultMessageProps
    , Direction(..)
    , newHeaders
    ) where

import GenericUtilities (unWordsBy)

data Direction = Incoming | Outgoing deriving Show
data MessageProps = MessageProps {
       m_message                :: String,
       m_id                     :: Maybe Int,
       m_sessionInfo            :: SessionInfo,
       m_method                 :: Maybe String,
       m_handle                 :: Maybe Int,
       m_assignedHandle         :: Maybe Int,
       m_objectInfo             :: Maybe ObjectInfo,
       m_direction              :: Maybe Direction,
       m_hasResponse            :: Maybe Bool,
       m_isError                :: Bool,
       m_timeStamp              :: String,
       m_timeStampRequest       :: Maybe String,
       m_transactionsInProgress :: Int
     }

defaultMessageProps :: String -> String -> MessageProps
defaultMessageProps msg ts = MessageProps {
       m_message                = msg,
       m_id                     = Nothing,
       m_sessionInfo            = emptySession,
       m_method                 = Nothing,
       m_handle                 = Nothing,
       m_assignedHandle         = Nothing,
       m_objectInfo             = Nothing,
       m_direction              = Nothing,
       m_hasResponse            = Nothing,
       m_isError                = False,
       m_timeStamp              = ts,
       m_timeStampRequest       = Nothing,
       m_transactionsInProgress = 0
     }

data ObjectInfo = ObjectInfo {
    o_id          :: String,
    o_type        :: String,
    o_genericType :: Maybe String
  }

instance Show ObjectInfo where
  show (ObjectInfo objectId typ genericType) = unWordsBy '\t' [objectId, typ, maybe "" id genericType]

data SessionInfo = SessionInfo { 
       s_sessionId :: String,
       s_userDir   :: String,
       s_userId    :: String
     }

instance Show SessionInfo where
  show (SessionInfo id dir usr) = unWordsBy '\t' [id, dir, usr]
     
emptySession :: SessionInfo
emptySession = SessionInfo "" "" ""

newHeaders = [ "Direction"
             , "HasResponse"
             , "Method"
             , "Handle"
             , "AssignedHandle"
             , "ObjectId"
             , "ObjectType"
             , "GenericObjectType"
             , "MsgId"
             , "IsError"
             , "TimeStampRequest"
             , "TransactionsInProgress"
             , "SessionId"
             , "SessionUserDir"
             , "SessionUserId"
             ]

instance Show MessageProps where
  show msgProps = unWordsBy '\t' $ map ($msgProps) [ printMaybe m_direction        show ""
                                                   , printMaybe m_hasResponse      show ""
                                                   , printMaybe m_method           id   ""
                                                   , printMaybe m_handle           show ""
                                                   , printMaybe m_assignedHandle   show ""
                                                   , printMaybe m_objectInfo       show "\t\t"
                                                   , printMaybe m_id               show ""
                                                   , show.m_isError
                                                   , printMaybe m_timeStampRequest id   ""
                                                   , show.m_transactionsInProgress
                                                   , show.m_sessionInfo
                                                   ]
   where
     printMaybe :: (MessageProps -> Maybe a) -> (a -> String) -> String -> MessageProps -> String
     printMaybe f showF base p = maybe base showF (f p)
