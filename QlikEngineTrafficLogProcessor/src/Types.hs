module Types
    ( MessageProps(..)
    , defaultMessageProps
    , Direction(..)
    ) where

import GenericUtilities (unWordsBy)

data Direction = Incoming | Outgoing deriving Show
data MessageProps = MessageProps {
       m_message                :: String,
       m_id                     :: Maybe Int,
       m_method                 :: Maybe String,
       m_handle                 :: Maybe Int,
       m_direction              :: Maybe Direction,
       m_isError                :: Bool,
       m_timeStamp              :: String,
       m_timeStampRequest       :: Maybe String,
       m_transactionsInProgress :: Int
     }

defaultMessageProps :: String -> String -> MessageProps
defaultMessageProps msg ts = MessageProps {
       m_message                = msg,
       m_id                     = Nothing,
       m_method                 = Nothing,
       m_handle                 = Nothing,
       m_direction              = Nothing,
       m_isError                = False,
       m_timeStamp              = ts,
       m_timeStampRequest       = Nothing,
       m_transactionsInProgress = 0
     }

instance Show MessageProps where
  show msgProps = unWordsBy ('\t') $ map ($msgProps) [ printMaybe m_direction show
                                                     , printMaybe m_method    id
                                                     , printMaybe m_handle    show
                                                     , printMaybe m_id        show
                                                     , show.m_isError
                                                     , printMaybe m_timeStampRequest id
                                                     , show.m_transactionsInProgress
                                                     ]
   where printMaybe :: (MessageProps -> Maybe a) -> (a -> String) -> MessageProps -> String
         printMaybe f showF p = maybe "" showF (f p)
