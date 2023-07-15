module Main where

import Lib

import Colog
import Options.Applicative
import Data.Aeson (encodeFile)

main :: IO ()
main = do
  command <- execParser (info commandParser (progDesc "Busboy data collector"))
  case command of
    RunBusboyApp databasePath logFile -> withLogTextFile logFile (runBusboyApp databasePath)
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
    strOption (long "log-file" <> short 'l' <> metavar "/path/to/log.txt")


getRouteStopMapParser :: Parser Command
getRouteStopMapParser =
  GetRouteStopMap <$>
    argument str (metavar "outputFile.json")

data Command
  = RunBusboyApp FilePath FilePath
  | GetRouteStopMap FilePath
