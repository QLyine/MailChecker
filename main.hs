module Main ( main, computeOneMail ) where

import Control.Arrow (first)
import Control.Monad
import Control.Applicative

import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe ( fromMaybe )

import System.Environment
import System.Console.GetOpt
import System.Directory
import System.FilePath
import System.Exit (exitWith, ExitCode(ExitSuccess))

data Options = Options
 { optVerbose     :: Bool
 , optShowVersion :: Bool
 } deriving Show

defaultOptions    = Options
 { optVerbose     = False
 , optShowVersion = False
 }

data MailFile = MailFile {
    from      :: String
  , to        :: String 
  , subject   :: String
  , date      :: String 
  } deriving Show

data Mail = Mail {
    mbox      :: String 
  , mailFile  :: [MailFile]
  , count     :: Int
  } deriving Show

main :: IO ()
main = getArgs >>= compilerOpts >>= compute >>= printMail

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v']     ["verbose"]
     (NoArg (\ opts -> opts { optVerbose = True }))
     "enable verbose messages"
 , Option ['V','?'] ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True }))
     "show version number"
 ]

compilerOpts :: [String] -> IO (Options, [FilePath])
compilerOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

header :: String
header = "Usage: mailReader [OPTION...] files..."

compute :: (Options, [FilePath]) -> IO [Mail]
compute (opt, dirList)
  | (optShowVersion opt) = version >> exit
  | null dirList = putStrLn "SomeError" >> exit
  | not (null dirList) = mapM (computeOneMail (fileToParse) ) dirList

computeOneMail :: (FilePath -> IO (MailFile)) -> [Char] -> IO Mail
computeOneMail f fpath = do
      let mbox  = snd $ splitFileName fpath 
          dir   = fpath </> "new"  
      filesToProcess <- (getDirectoryContents dir) 
            >>= mapM ( canonicalizePath . (dir </>))
            . filter (`notElem` [".", ".."] ) 
      print $ filesToProcess
      let count = length filesToProcess 
      mailFiles <- mapM ( f ) filesToProcess
      return $ Mail mbox mailFiles count

version = putStrLn "MailReader 0.1"

exit = exitWith ExitSuccess

conds :: BC.ByteString -> Bool
conds bs = B.isPrefixOf (BC.pack "From:") bs 
  || B.isPrefixOf (BC.pack "Subject:") bs
  || B.isPrefixOf (BC.pack "Date:") bs
  || B.isPrefixOf (BC.pack "To:") bs


getValue :: [BC.ByteString] -> String -> String
getValue fields str = 
    BC.unpack 
  . snd  
  . B.break (==BI.c2w (' ')) 
  . head 
  $ filter (B.isPrefixOf (BC.pack str)) fields

fileToParse :: FilePath -> IO MailFile
fileToParse f = do
  fileLines <- B.split (BI.c2w ('\n')) <$> B.readFile f
  let fields  = filter conds fileLines
      from    = getValue fields "From"
      to      = getValue fields "To"
      date    = getValue fields "Date"
      sub     = getValue fields "Subject"
  return $ MailFile from to sub date

printMail :: [Mail] -> IO () 
printMail [] = putStrLn "End of Check"
printMail (x:xs) = do
  putStrLn $ ">>>>>> " ++ (mbox x) ++ " >>>>>>"
  --putStrLn $ "Unread >> " ++ (count x) 
  putStrLn $ printMailFile (mailFile x) 
  putStrLn $ "<<<<<< " ++ (mbox x) ++ " <<<<<<\n"
  printMail xs 
  
printMailFile :: [MailFile] -> String
printMailFile [] = ""
printMailFile (x:xs) = 
      "\tFrom >> " ++ (from x) ++ "\n"
  ++  "\tTo >> " ++ (to x) ++ "\n"
  ++  "\tSubject >> " ++ (subject x) ++ "\n"
  ++  "\tDate >> " ++ (date x) ++ "\n\n"
  ++  printMailFile xs 
