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
       i_timeStamp :: Int
     }

findIndexes :: String -> MessageIndexes
findIndexes line =
  let ws = wordsBy ('\t') line
   in MessageIndexes (fromJust $ findIndex (=="Message") ws)
                     (fromJust $ findIndex (=="Timestamp") ws)
          
processBody :: MessageIndexes -> [String] -> [String]
processBody indexes body =
  let msgProps = map (processBodyLine indexes) body
      seqProps = processBodySequence ([],0) msgProps
   in zipWith (\s0 s1 -> s0 ++ "\t" ++ s1) body $ map show (snd seqProps)

processBodyLine :: MessageIndexes -> String -> MessageProps
processBodyLine indexes line =
  let columns     = wordsBy ('\t') line
      msg         = columns !! (i_message indexes)
      ts          = columns !! (i_timeStamp indexes)
      m_dir       = getDir msg
      baseMessage = (defaultMessageProps msg ts) { m_direction = m_dir }
   in case m_dir of
        Nothing -> baseMessage
        Just _  -> processJson (drop 4 msg) baseMessage

type SeqState = ([(Int, (String, String))], Int)

processBodySequence :: SeqState -> [MessageProps] -> (SeqState, [MessageProps])
processBodySequence state [] = (state, [])
processBodySequence state (m:ms) = let (newState, newM) = processBodyProps state m
                                       (finalState, newMs) = processBodySequence newState ms
                                    in (finalState, newM:newMs)

is :: (a -> b) -> (b -> Bool) -> a -> Bool
is f p = p.f

processBodyProps :: SeqState -> MessageProps -> (SeqState, MessageProps)
processBodyProps (requests, cnt) props =
  case m_id props of
    Nothing -> ((requests, cnt), props { m_transactionsInProgress = cnt })
    Just id -> case (m_method props, m_direction props) of
      (Just method, Nothing) -> error ("Method without direction: " ++ method ++ " #" ++ m_timeStamp props)
      (Just method, Just Incoming) ->
        let newRequest = (id, (m_timeStamp props, method))
            newCnt     = cnt + 1
         in ((newRequest:requests, newCnt), props { m_transactionsInProgress = newCnt })
      (Nothing, Just Outgoing) ->
        case map snd $ filter (fst `is` (==id)) requests of
          [] -> trace ("Response for unknown request: " ++ show id ++ " #" ++ m_timeStamp props)
                      ((requests, cnt), props { m_transactionsInProgress = cnt })
          [(timestamp, method)] ->
            let newRequests = filter (fst `is` (/=id)) requests
                newCnt      = cnt - 1
             in ( (newRequests, newCnt)
                , props { m_timeStampRequest       = Just timestamp
                        , m_method                 = Just method
                        , m_transactionsInProgress = newCnt
                        }
                )
          timestamps ->
            trace ("Response for multiple concurrent requests: " ++ show id ++ " Assuming youngest #" ++ m_timeStamp props) $
              let (matching, others) = partition (fst `is` (==id)) requests
                  (timestamp, method) = snd $ head matching
                  newRequests = (tail matching) ++ others
                  newCnt      = cnt - 1
               in ( (newRequests, newCnt)
                  , props { m_timeStampRequest       = Just timestamp
                          , m_method                 = Just method
                          , m_transactionsInProgress = newCnt
                          }
                  )
      (Nothing, Nothing) -> ((requests, cnt), props { m_transactionsInProgress = cnt })
    

processHeader :: String -> String
processHeader line =
  let headers = wordsBy '\t' line
      newHeaders = headers ++ ["Direction", "Method", "Handle", "MsgId", "IsError", "TimeStampRequest", "TransactionsInProgress" ]
   in unWordsBy '\t' newHeaders

{-
processBody :: (State, String) -> (State, String)
processBody msgIndex line =
  let columns = wordsBy '\t' line
      msg = columns !! msgIndex
      dir = getDir msg
      props = if dir == nullValue
              then [nullValue, nullValue]
--              else ["apa", "apa"]
              else let m_json = readObject (drop 4 msg)
                    in map (getJsonProperty (fromJust m_json)) ["method", "id"]
      newColumns = columns ++ (dir:props)
   in unWordsBy '\t' newColumns

nullValue = ""
-}
getDir line = case take 3 line of
                "<<<" -> Just Outgoing
                ">>>" -> Just Incoming
                _     -> Nothing
       
