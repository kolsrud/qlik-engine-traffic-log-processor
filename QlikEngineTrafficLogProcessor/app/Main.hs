module Main where

import Lib
import Types
import GenericUtilities
import Debug.Trace
import System.Environment
import Data.Maybe
import Data.List
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as Map

main :: IO ()
main = do args <- getArgs
          if null args then
            putStrLn "Usage: TrafficLogProcessor.exe <traffic log file name>"
           else do
            let fileName = (head args)
            putStrLn $ "Processing file " ++ fileName
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
      (_, seqProps) = processBodySequence ([], Map.empty, 0) msgProps
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

type SeqState = ([(Int, RequestInfo)], HashMap Int ObjectInfo, Int)

processBodySequence :: SeqState -> [MessageProps] -> (SeqState, [MessageProps])
processBodySequence state [] = (state, [])
processBodySequence state (m:ms) = let (newState, newM) = processBodyProps state m
                                       (finalState, newMs) = processBodySequence newState ms
                                    in (finalState, newM:newMs)

is :: (a -> b) -> (b -> Bool) -> a -> Bool
is f p = p.f

processBodyProps :: SeqState -> MessageProps -> (SeqState, MessageProps)
processBodyProps state@(_,_,cnt) props =
  case m_id props of
    Nothing -> (state, props { m_transactionsInProgress = cnt })
    Just id -> case (m_method props, m_direction props) of
      (Just method, Nothing) -> error ("Method without direction: " ++ method ++ " @" ++ m_timeStamp props)
      (Just method, Just Incoming) -> incomingRequest id method state props
      (Nothing,     Just Outgoing) -> outgoingResponse id state props
      (Nothing, Nothing) -> (state, props { m_transactionsInProgress = cnt })
 where
  incomingRequest :: Int -> String -> SeqState -> MessageProps -> (SeqState, MessageProps)
  incomingRequest reqId method (requests, handleInfo, cnt) props =
      case m_handle props of
         Nothing -> error ("Incoming method without handle: " ++ method ++ " @" ++ m_timeStamp props)
         Just handle ->
           let newRequest = (reqId, RequestInfo (m_timeStamp props) method handle)
               newCnt     = cnt + 1
            in ( (newRequest:requests, handleInfo, newCnt)
               , props { m_transactionsInProgress = newCnt
                       , m_objectInfo = if handle == -1
                                        then Just hubObjectInf
                                        else Map.lookup handle handleInfo
                       }
               )

  hubObjectInf = ObjectInfo "" "Hub" Nothing
 
  outgoingResponse :: Int -> SeqState -> MessageProps -> (SeqState, MessageProps)
  outgoingResponse reqId (requests, handleInfo, cnt) props =
        case partition (fst `is` (==reqId)) requests of
          ([],_) -> trace ("Response for unknown request: " ++ show reqId ++ " @" ++ m_timeStamp props)
                          ((requests, handleInfo, cnt), props { m_transactionsInProgress = cnt })
          (m:ms, others) ->
            let RequestInfo timestamp method handle =
                  if (not $ null ms) then
                    trace ( "Response for multiple concurrent requests: " ++ show reqId ++
                            " Assuming youngest @" ++ m_timeStamp props
                          ) (snd m)
                  else snd m
                newHandleInfo = maybe handleInfo
                                      (\(assignedHandle, objectInfo) -> Map.insert assignedHandle objectInfo handleInfo)
                                      (do assignedHandle <- m_assignedHandle props
                                          objectInfo     <- m_objectInfo     props
                                          return (assignedHandle, objectInfo)
                                      )
                newCnt        = cnt - 1
             in ( (ms ++ others, newHandleInfo, newCnt)
                , props { m_timeStampRequest       = Just timestamp
                        , m_method                 = Just method
                        , m_handle                 = Just handle
                        , m_objectInfo             = if handle == -1
                                                     then Just hubObjectInf
                                                     else Map.lookup handle newHandleInfo
                        , m_transactionsInProgress = newCnt
                        }
                )

processHeader :: String -> String
processHeader line =
  let headers = wordsBy '\t' line
   in unWordsBy '\t' (headers ++ newHeaders)

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
      Nothing -> error ("Message without direction: @" ++ m_timeStamp props)
      Just Incoming -> incomingRequest id (m_sessionInfo props) state props
      Just Outgoing -> ((id,m_sessionInfo props):state, props)
 where
  incomingRequest :: Int -> SessionInfo -> SessionState -> MessageProps -> (SessionState, MessageProps)
  incomingRequest msgId sessionInfo state props = case partition (fst `is` (==msgId)) state of
    ([],_) -> trace ("Encountered request without response: " ++ (show (m_method props)))
                    (state, props { m_hasResponse = Just False })
    ([(_, info)], newState) -> (newState, props { m_sessionInfo = info, m_hasResponse = Just True })
    (((_, info):infos), newState) ->
      trace ("Multiple requests for response with ID " ++ show msgId ++ ". Assuming youngest @" ++ m_timeStamp props)
            (infos ++ newState, props { m_sessionInfo = info, m_hasResponse = Just True })
   
