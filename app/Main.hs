{-# LANGUAGE BlockArguments #-}
module Main where

import Lib

import Options.Applicative
import Data.Aeson (encodeFile)
import Colog (LogAction(..), logTextHandle, logFlush)
import System.IO (withFile, IOMode (AppendMode))

main :: IO ()
main = do
  command <- execParser (info commandParser (progDesc "Busboy data collector"))
  case command of
    RunBusboyApp databasePath logTarget ->
      case logTarget of
        NoLog -> runBusboyApp databasePath (LogAction \_ -> return ())
        LogToFile logFile ->
          withFile logFile AppendMode \logHandle ->
            runBusboyApp databasePath (logTextHandle logHandle <> logFlush logHandle)
    GetRouteStopMap outFile -> writeRouteStopMaps outFile

commandParser :: Parser Command
commandParser = subparser
  ( command "runBusboyApp" (info runBusboyAppParser (progDesc "Run busboy app"))
  <> command "getRouteStopMap" (info getRouteStopMapParser (progDesc "Get route stop map, output to JSON"))
  )

runBusboyAppParser :: Parser Command
runBusboyAppParser =
  RunBusboyApp <$>
    argument str (metavar "/path/to/database.db") <*>
    logTarget

data LogTarget
  = LogToFile FilePath
  | NoLog

logTarget :: Parser LogTarget
logTarget = fmap LogToFile logFile <|> pure NoLog
  where
    logFile :: Parser FilePath
    logFile = strOption (long "log-file" <> short 'l' <> metavar "/path/to/log.txt")

getRouteStopMapParser :: Parser Command
getRouteStopMapParser =
  GetRouteStopMap <$>
    argument str (metavar "outputFile.json")

data Command
  = RunBusboyApp FilePath LogTarget
  | GetRouteStopMap FilePath
