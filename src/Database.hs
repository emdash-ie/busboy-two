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
  \ , lastModified text \
  \ , scheduledArrivalTime text null \
  \ , actualOrEstimatedArrivalTime text null \
  \ , scheduledDepartureTime text null \
  \ , actualOrEstimatedDepartureTime text null \
  \ , primary key (retrievedAt, passageId) \
  \ )"

createLocationTable :: Query
createLocationTable =
  "create table if not exists locations \
  \ ( retrievedAt text \
  \ , vehicleId text \
  \ , latitude integer \
  \ , longitude integer \
  \ , bearing smallint \
  \ , congestionLevel tinyint \
  \ , accuracyLevel tinyint \
  \ , lastModified text \
  \ , primary key (retrievedAt, vehicleId) \
  \ )"

createVehiclesTable :: Query
createVehiclesTable =
  "create table if not exists vehicles \
  \ ( retrievedAt text \
  \ , vehicleId text \
  \ , isAccessible boolean \
  \ , hasBikeRack boolean \
  \ )"

createPassagesTable :: Query
createPassagesTable =
  "create table if not exists passages \
  \ ( id text \
  \ , retrievedAt text \
  \ , tripId text \
  \ , routeId text \
  \ , stopId text \
  \ , vehicleId text \
  \ )"
