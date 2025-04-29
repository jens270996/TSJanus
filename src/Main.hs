
import System.Environment
import System.Exit
import System.Timeout
import Control.Monad
import Data.List

-- import Jana.ParserBasic
import Jana.Parser
import Jana.Eval (runProgram)
import Jana.SymmetryChecker (checkSymmetryId)
import Jana.Types (defaultOptions, EvalOptions(..), DebugMode(..), ModEval(..))
import Jana.Invert
import qualified Jana.JanusToC as JTC


data Options = Options
  { timeOut :: Int
  , invert :: Bool
  , ast :: Bool
  , cCode  :: Bool
  , header :: Maybe String
  , symmetricProcedure :: Maybe String
  , checkSymmetry :: Bool
  , evalOpts :: EvalOptions }

defaults :: Options
defaults = Options
  { timeOut = -1
  , invert = False
  , ast = False
  , cCode = False
  , header = Nothing
  , symmetricProcedure = Nothing
  , checkSymmetry = False
  , evalOpts = defaultOptions }

usage :: String
usage = "usage: jana [options] <file>\n\
        \options:\n\
        \  -m[n]        use n-bit modular arithmetic; if (n) is unset 32 is used\n\
        \  -p[n]        use GF(n) finite field arithmetic; if (n) is unset M_31 (2147483647) is used\n\
        \  -tN          timeout after N seconds\n\
        \  -i           print inverted program\n\
        \  -c           print C++ program\n\
        \  -a           print program AST (useful for debugging)\n\
        \  -h=file.h    header files to be included in translation to C++\n\
        \  -s=symmetricProcedure name of procedure to check symmetry on\n\
        \                 this header files is intended use with external functions\n\
        \  -d           interactive debug mode\n\
        \  -e           enter debug mode on error\n\
        \                 (type \"h[elp]\" for options)"

parseArgs :: IO (Maybe ([String], Options))
parseArgs =
  do args <- getArgs
     (flags, files) <- return $ splitArgs args
     case checkFlags flags of
       Left err   -> putStrLn err >> return Nothing
       Right opts -> return $ Just (files, opts)

splitArgs :: [String] -> ([String], [String])
splitArgs = partition (\arg -> head arg == '-' && length arg > 1)

checkFlags :: [String] -> Either String Options
checkFlags = foldM addOption defaults

addOption :: Options -> String -> Either String Options
addOption opts@(Options { evalOpts = evalOptions }) "-m" =
  return $ opts { evalOpts = evalOptions { modInt = (ModPow2 32) } }
addOption opts@(Options { evalOpts = evalOptions }) ('-':'m':n) =
  case reads n of
    [(nVal, "")] -> return $ opts { evalOpts = evalOptions { modInt = (ModPow2 nVal) } }
    _               -> Left "Non-number given to -m option"
addOption opts@(Options { evalOpts = evalOptions }) "-p" =
  return $ opts { evalOpts = evalOptions { modInt = (ModPrime 2147483647) } }
addOption opts@(Options { evalOpts = evalOptions }) ('-':'p':n) =
  case reads n of
    [(nVal, "")] ->
      if isPrime nVal
      then return $ opts { evalOpts = evalOptions { modInt = (ModPrime nVal) } }
      else Left "Non-prime given to -p option"
    _            -> Left "Non-number given to -p option"
addOption opts ('-':'t':time) =
  case reads time of
    [(timeVal, "")] -> return $ opts { timeOut = timeVal }
    _               -> Left "Non-number given to -t option"
addOption opts "-a" = return opts { ast = True }
addOption opts "-i" = return opts { invert = True }
addOption opts "-c" = return opts { cCode = True }
addOption opts ('-':'h':'=':headerfile) = return opts { header = Just headerfile }
addOption opts ('-':'s':'=':symmetricProcedure) = return opts { symmetricProcedure = Just symmetricProcedure, checkSymmetry = True }
addOption opts@(Options { evalOpts = evalOptions }) "-d" =
 return $ opts { evalOpts = evalOptions {runDebugger = DebugOn } }
addOption opts@(Options { evalOpts = evalOptions }) "-e" =
 return $ opts { evalOpts = evalOptions {runDebugger = DebugError } }
addOption _ f = Left $ "invalid option: " ++ f

loadFile :: String -> IO String
loadFile "-"      = getContents
loadFile filename = readFile filename

printInverted :: String -> IO ()
printInverted filename =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print $ invertProgram prog

printAST :: String -> IO ()
printAST filename =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print prog

printCcode :: String -> Maybe String -> IO ()
printCcode filename headerfile =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print $ JTC.formatProgram headerfile prog

printInvertedCcode :: String -> Maybe String -> IO ()
printInvertedCcode filename headerfile =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print $ JTC.formatProgram headerfile $ invertProgram prog

printSymmetryChecker :: String -> Maybe String -> IO ()
printSymmetryChecker filename (Just procId) =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> (if checkSymmetryId prog procId
                     then print ("Procedure " ++ procId ++ " is time symmetric.")
                     else print ("Procedure " ++ procId ++ " is not time symmetric."))
                      >> exitSuccess
printSymmetryChecker _ _ = print "No procedure name supplied to check symmetry on" >> (exitWith $ ExitFailure 1)
parseAndRun :: String -> EvalOptions -> IO ()
parseAndRun filename evalOptions =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> runProgram filename prog evalOptions

main :: IO ()
main = do args <- parseArgs
          case args of
            Just ([file], Options { cCode = True, invert = True, header = h }) -> printInvertedCcode file h
            Just ([file], Options { cCode = True, header = h }) -> printCcode file h
            Just ([file], Options { symmetricProcedure=sp, checkSymmetry= True }) -> printSymmetryChecker file sp
            Just ([file], Options { ast = True }) -> printAST file
            Just ([file], Options { invert = True }) -> printInverted file
            Just ([file], opts) ->
              do res <- timeout (timeOut opts * 1000000)
                                (parseAndRun file (evalOpts opts))
                 case res of
                   Nothing -> exitWith $ ExitFailure 124
                   _       -> return ()
            _ -> putStrLn usage

isPrime :: Int -> Bool
isPrime k = (k > 1) && null [ x | x <- [2.. k-1], k `mod` x == 0]