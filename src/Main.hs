import System.IO
import System.Environment (getArgs)
import Control.Exception ( throw, Exception )
import Control.Monad ( when, unless )
--import Data.Char (isSpace)
import Data.List ( intercalate )

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

helpMessage :: String
helpMessage = "placeholder"

possibleFlags :: String
possibleFlags = "h"
possibleLFlags :: [String]
possibleLFlags = ["help"]

-- If the list is empty, return "", else it'll return the first element
firstOrEmpty :: [String] -> String
firstOrEmpty []         = ""
firstOrEmpty (str:strs) = str

checkFlags :: [Char] -> Bool
checkFlags ""       = True
checkFlags (f:fs)   = f `elem` possibleFlags && checkFlags fs

checkLFlags :: [String] -> Bool
checkLFlags []          = True
checkLFlags (lf:lfs)    = lf `elem` possibleLFlags && checkLFlags lfs

isFlag :: String -> Bool
isFlag str = head str == '-'

removeDash :: String -> String
removeDash str = case str of
    (_:cs) -> cs

isLongFlag :: String -> Bool
isLongFlag str = case str of
    (c1:c2:cs) -> [c1, c2] == "--"
    _ -> False

remove2Dash :: String -> String
remove2Dash str = case str of
    (_:_:cs) -> cs

parseArgs :: [String] -> ([String], String, [String])
parseArgs []            = throw (Error "Error: No arguments specified")
parseArgs strs = helper strs [] "" []
    where
        helper args argv flags longFlags = case args of
            (a:as)
                | isLongFlag a  -> helper as  argv            flags                   (longFlags ++ [remove2Dash a])
                | isFlag a      -> helper as  argv           (flags ++ removeDash a)   longFlags
                | otherwise     -> helper as (argv ++ [a])    flags                    longFlags
            []                  -> (argv, flags, longFlags)

main :: IO ()
main = do
    args <- getArgs
    let (argv, flags, longFlags) = parseArgs args

    if ('h' `elem` flags) || ("help" `elem` longFlags) then do
        putStrLn helpMessage
    else do
        when (null argv)                $ throw (Error "Error: No arguments specified")
        unless (checkFlags flags)       $ throw (Error "Error: Invalid flag")
        unless (checkLFlags longFlags)  $ throw (Error "Error: Invalid long flag")

        let (filePath:arguments) = argv

        {-
        print ("filePath: " ++ filePath)
        print ("arguments: " ++ concat arguments)
        print ("flags: " ++ flags)
        print ("longFlags: " ++ concat longFlags)
        -}

        rawText <- readFile filePath

        putStr rawText
