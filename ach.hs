import System.FilePath.Find (find, always, 
                             (==?),
                             fileType, FileType(..) )
import Control.Monad        (filterM)
import Data.Maybe           (catMaybes)

import qualified Data.Dequeue as D
                            (empty, BankersDequeue, 
                             pushBack, popFront, 
                             fromList,
                             length)
import Data.Foldable        (toList)

type GrepRecord = (FilePath, [GrepLine])
type GrepLine = (Int, String)

-- grep a list of lines
grep p f ls = 
    let nls = zip [1..] ls
        gs = filter (p . snd) $ nls
    in case gs of
        []        -> Nothing
        otherwise -> Just (f, gs)

-- for example:
-- grepc (=="FOO") 2 2 
--  ["bar", "bar", "FOO", "bar", "bar", 
--   "bar", "bar", "FOO", "bar", "FOO", "bar", "FOO", "bar", "bar", 
--   "bar", "FOO", "bar", "bar", "bar"]

grepc p b a ls = let before = grepcTill p b ls
                 in case before of
                    Nothing                 -> []
                    Just (before, l, after) -> 
                        let (after', rest) = grepcAfter p a after
                            record = (toList before) ++ [l] ++ after'
                        in case rest of
                            []        -> [record]
                            otherwise -> record : grepc p b a rest
                            
grepcTill p b ls =
    let g bs (l:ls) = if (p l) 
                      then Just (bs, l, ls)
                      else let bs' = addQ b bs l
                           in g bs' ls
        g _ [] = Nothing
    in g bd ls

grepcAfter p a ls =
    let (after, rest) = span (not.p) ls
        (after', r) = splitAt a after
        rest' = r ++ rest
    in if (length after') == a then (after', rest')
       else case rest' of
            (r:rs)    -> let (rafter, rests) = (grepcAfter p a rs)
                         in (after' ++ [r] ++ rafter, rests)
            otherwise -> (after', rest')

bd = D.empty :: D.BankersDequeue a

addQ :: Int -> D.BankersDequeue a -> a -> D.BankersDequeue a
addQ b bs l = let bs' = D.pushBack bs l
              in if (D.length bs') > b then
                    let (_, tailQ) = D.popFront bs'
                    in tailQ
                  else bs'

-- grep a single file
grepF :: (String -> Bool) -> String -> IO (Maybe GrepRecord)
grepF p f = do
    contents <- readFile f
    let ls = lines contents
    return $ grep p f ls

-- grep a list of files
grepFs :: (String -> Bool) -> [String] -> IO [GrepRecord]
grepFs p fs = mapM (grepF p) fs >>= return . catMaybes

-- recursive grep on a path
ack :: (String -> Bool) -> FilePath -> IO [GrepRecord]
ack p path = 
    let isRegularFile = fileType ==? RegularFile
    in do
        fs  <- find always isRegularFile path
        grepFs p fs
