{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.SQLite.Simple

createTables :: Connection -> IO ()
createTables connection = do
  execute_ connection createPredictionTable
  execute_ connection createLocationTable
  execute_ connection createVehiclesTable
  execute_ connection createPassagesTable

createPredictionTable :: Query
createPredictionTable =
  "create table if not exists predictions \
  \ ( retrievedAt text \
  \ , passageId text \
  \ , stopId text \
  \ , lastModified text \
  \ , scheduledArrivalTime text null \
  \ , actualOrEstimatedArrivalTime text null \
  \ , scheduledDepartureTime text null \
  \ , actualOrEstimatedDepartureTime text null \
  \ , primary key (retrievedAt, passageId, stopId) \
  \ )"

createLocationTable :: Query
createLocationTable =
  "create table if not exists locations \
  \ ( retrievedAt text \
  \ , vehicleId text \
  \ , latitude integer \
  \ , longitude integer \
  \ , bearing smallint \
  \ , congestionLevel tinyint null \
  \ , accuracyLevel tinyint null \
  \ , lastModified text \
  \ )"

createVehiclesTable :: Query
createVehiclesTable =
  "create table if not exists vehicles \
  \ ( retrievedAt text \
  \ , vehicleId text \
  \ , isAccessible boolean null \
  \ , hasBikeRack boolean null \
  \ )"

createPassagesTable :: Query
createPassagesTable =
  "create table if not exists passages \
  \ ( id text \
  \ , retrievedAt text \
  \ , tripId text \
  \ , routeId text \
  \ , stopId text \
  \ , vehicleId text null \
  \ )"
