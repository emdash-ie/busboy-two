module Main where

import Lib

import Options.Applicative
import Data.Aeson (encodeFile)

main :: IO ()
main = do
  command <- execParser (info commandParser (progDesc "Busboy data collector"))
  case command of
    RunBusboyApp -> runBusboyApp
    GetRouteStopMap outFile -> writeRouteStopMaps outFile

commandParser :: Parser Command
commandParser = subparser
  ( command "runBusboyApp" (info (pure RunBusboyApp) (progDesc "Run busboy app"))
  <> command "getRouteStopMap" (info getRouteStopMapParser (progDesc "Get route stop map, output to JSON"))
  )

getRouteStopMapParser :: Parser Command
getRouteStopMapParser =
  GetRouteStopMap <$>
    argument str (metavar "outputFile.json")

data Command
  = RunBusboyApp
  | GetRouteStopMap FilePath
