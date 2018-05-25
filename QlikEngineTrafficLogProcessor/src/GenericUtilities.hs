module GenericUtilities
    ( wordsBy
    , unWordsBy
    ) where

import Data.List (intercalate)

wordsBy :: Char -> String -> [String]
wordsBy _ [] = []
wordsBy sep str = let (w,rest) = break (==sep) str
                   in w:( if null rest
                          then []
                          else wordsBy sep (tail rest) )

unWordsBy :: Char -> [String] -> String
unWordsBy sep strs = intercalate [sep] strs
