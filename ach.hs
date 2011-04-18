import System.FilePath.Find (find, always, 
                             (==?),
                             fileType, FileType(..) )
import Control.Monad        (filterM)
import Data.Maybe           (catMaybes)

type GrepRecord = (FilePath, [GrepLine])
type GrepLine = (Int, String)

-- grep a single file
grep :: (String -> Bool) -> String -> IO (Maybe GrepRecord)
grep p f = do
    putStr f
    contents <- readFile f
    let ls = filter (p . snd) $ zip [1..] (lines contents)
    case ls of
        []        -> return Nothing
        otherwise -> return $ Just (f, ls)

-- grep a list of files
grepF :: (String -> Bool) -> [String] -> IO [GrepRecord]
grepF p fs = mapM (grep p) fs >>= return . catMaybes

-- recursive grep on a path
ack :: (String -> Bool) -> FilePath -> IO [GrepRecord]
ack p path = 
    let isRegularFile = fileType ==? RegularFile
    in do
        fs  <- find always isRegularFile path
        grepF p fs
