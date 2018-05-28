module Main where

import Lib
import Types
import GenericUtilities
import Debug.Trace
import System.Environment
import Data.Maybe
import Data.List

main :: IO ()
main = do args <- getArgs
          putStrLn (show args)
          let fileName = (head args)
          txt  <- readFile fileName
          let newTxt = processFile txt
          writeFile ("New" ++ fileName) newTxt

processFile :: String -> String
processFile txt =
  let (header:body) = lines txt
      newHeader = processHeader header
      newBody   = processBody (findIndexes header) body
   in unlines (newHeader:newBody)

data MessageIndexes = MessageIndexes {
       i_message   :: Int,
       i_timeStamp :: Int,
       i_sessionId :: Int,
       i_userDir   :: Int,
       i_userId    :: Int
     }

findIndexes :: String -> MessageIndexes
findIndexes line =
  let ws = wordsBy ('\t') line
   in MessageIndexes (getIndex "Message"             ws)
                     (getIndex "Timestamp"           ws)
                     (getIndex "ProxySessionId"      ws)
                     (getIndex "ActiveUserDirectory" ws)
                     (getIndex "ActiveUserId"        ws)

 where
  getIndex header headers = fromJust $ findIndex (==header) headers
  
processBody :: MessageIndexes -> [String] -> [String]
processBody indexes body =
  let msgProps      = map (processBodyLine indexes) body
      (_, seqProps) = processBodySequence ([],0) msgProps
      withSession   = reverse $ snd $ processBodySession [] (reverse seqProps)
   in zipWith (\s0 s1 -> s0 ++ "\t" ++ s1) body $ map show withSession

processBodyLine :: MessageIndexes -> String -> MessageProps
processBodyLine indexes line =
  let columns     = wordsBy ('\t') line
      msg         = columns !! (i_message indexes)
      ts          = columns !! (i_timeStamp indexes)
      sessionInfo = getSessionInfo indexes columns
      m_dir       = getDir msg
      baseMessage = (defaultMessageProps msg ts) { m_direction = m_dir, m_sessionInfo = sessionInfo }
   in case m_dir of
        Nothing -> baseMessage
        Just _  -> processJson (drop 4 msg) baseMessage
 where
  getSessionInfo :: MessageIndexes -> [String] -> SessionInfo
  getSessionInfo indexes columns =
    let sessionId = columns !! (i_sessionId indexes)
        userDir   = columns !! (i_userDir   indexes)
        userId    = columns !! (i_userId    indexes)
     in SessionInfo sessionId userDir userId
  
data RequestInfo = RequestInfo { r_timestamp :: String
                               , r_method    :: String
                               , r_handle    :: Int
                               }

type SeqState = ([(Int, RequestInfo)], Int)

processBodySequence :: SeqState -> [MessageProps] -> (SeqState, [MessageProps])
processBodySequence state [] = (state, [])
processBodySequence state (m:ms) = let (newState, newM) = processBodyProps state m
                                       (finalState, newMs) = processBodySequence newState ms
                                    in (finalState, newM:newMs)

is :: (a -> b) -> (b -> Bool) -> a -> Bool
is f p = p.f

processBodyProps :: SeqState -> MessageProps -> (SeqState, MessageProps)
processBodyProps state props =
  case m_id props of
    Nothing -> (state, props { m_transactionsInProgress = snd state })
    Just id -> case (m_method props, m_direction props) of
      (Just method, Nothing) -> error ("Method without direction: " ++ method ++ " #" ++ m_timeStamp props)
      (Just method, Just Incoming) -> incomingRequest id method state props
      (Nothing,     Just Outgoing) -> outgoingResponse id state props
      (Nothing, Nothing) -> (state, props { m_transactionsInProgress = snd state })
 where
  incomingRequest :: Int -> String -> SeqState -> MessageProps -> (SeqState, MessageProps)
  incomingRequest reqId method (requests, cnt) props =
      case m_handle props of
         Nothing -> error ("Incoming method without handle: " ++ method ++ " #" ++ m_timeStamp props)
         Just handle ->
           let newRequest = (reqId, RequestInfo (m_timeStamp props) method handle)
               newCnt     = cnt + 1
            in ((newRequest:requests, newCnt), props { m_transactionsInProgress = newCnt })

  outgoingResponse :: Int -> SeqState -> MessageProps -> (SeqState, MessageProps)
  outgoingResponse reqId (requests, cnt) props =
        case map snd $ filter (fst `is` (==reqId)) requests of
          [] -> trace ("Response for unknown request: " ++ show reqId ++ " #" ++ m_timeStamp props)
                      ((requests, cnt), props { m_transactionsInProgress = cnt })
          [RequestInfo timestamp method handle] ->
            let newRequests = filter (fst `is` (/=reqId)) requests
                newCnt      = cnt - 1
             in ( (newRequests, newCnt)
                , props { m_timeStampRequest       = Just timestamp
                        , m_method                 = Just method
                        , m_handle                 = Just handle
                        , m_transactionsInProgress = newCnt
                        }
                )
          timestamps ->
            trace ("Response for multiple concurrent requests: " ++ show reqId ++ " Assuming youngest #" ++ m_timeStamp props) $
              let (matching, others) = partition (fst `is` (==reqId)) requests
                  RequestInfo timestamp  method handle = snd $ head matching
                  newRequests = (tail matching) ++ others
                  newCnt      = cnt - 1
               in ( (newRequests, newCnt)
                  , props { m_timeStampRequest       = Just timestamp
                          , m_method                 = Just method
                          , m_transactionsInProgress = newCnt
                          }
                  )

processHeader :: String -> String
processHeader line =
  let headers = wordsBy '\t' line
      newHeaders = headers ++ [ "Direction"
                              , "HasResponse"
                              , "Method"
                              , "Handle"
                              , "AssignedHandle"
                              , "MsgId"
                              , "IsError"
                              , "TimeStampRequest"
                              , "TransactionsInProgress"
                              , "SessionId"
                              , "SessionUserDir"
                              , "SessionUserId"
                              ]
   in unWordsBy '\t' newHeaders

getDir line = case take 3 line of
                "<<<" -> Just Outgoing
                ">>>" -> Just Incoming
                _     -> Nothing

type SessionState = [(Int, SessionInfo)]

processBodySession :: SessionState -> [MessageProps] -> (SessionState, [MessageProps])
processBodySession state [] = (state, [])
processBodySession state (m:ms) = let (newState, newM) = processBodySessionInstance state m
                                      (finalState, newMs) = processBodySession newState ms
                                   in (finalState, newM:newMs)

processBodySessionInstance :: SessionState -> MessageProps -> (SessionState, MessageProps)
processBodySessionInstance state props =
  case m_id props of
    Nothing -> (state, props)
    Just id -> case m_direction props of
      Nothing -> error ("Message without direction: #" ++ m_timeStamp props)
      Just Incoming -> incomingRequest id (m_sessionInfo props) state props
      Just Outgoing -> ((id,m_sessionInfo props):state, props)
 where
  incomingRequest :: Int -> SessionInfo -> SessionState -> MessageProps -> (SessionState, MessageProps)
  incomingRequest msgId sessionInfo state props = case partition (fst `is` (==msgId)) state of
    ([],_) -> trace ("Encountered request without response: " ++ (show (m_method props)))
                    (state, props { m_hasResponse = Just False })
    ([(_, info)], newState) -> (newState, props { m_sessionInfo = info, m_hasResponse = Just True })
   
