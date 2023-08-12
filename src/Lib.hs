{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( callStopPassage
    , callStopPoints
    , makeStopPointsHtml
    , runBusboyApp
    , getRouteStopMap
    , writeRouteStopMaps
    ) where

import Colog
import Control.Applicative (liftA2)
import Control.Category ((>>>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception hiding (Handler)
import Control.Lens ((^.), to, _1, views, (^..))
import Control.Monad (join, when, unless, guard)
import Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Fixed (Fixed(..))
import Data.Foldable (traverse_, for_)
import Data.Function ((&), on)
import Data.Functor (void)
import Data.Generics.Product (field)
import Data.Generics.Sum (_Ctor)
import Data.List (intersperse, sortOn)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Scientific (floatingOrInteger)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock (UTCTime, secondsToNominalDiffTime, getCurrentTime, addUTCTime, nominalDay, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector (Vector)
import Data.Vector.Lens (toVectorOf)
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple (SQLData(..))
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple.FromField as SQLite
import GHC.Generics (Generic)
import qualified Lucid
import Lucid hiding (type_, for_)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (throwError)
import Servant.Server (Handler, Server, serve, err404)
import Servant.API ((:>), (:<|>)(..), QueryParam, Get, JSON, ToHttpApiData(..), MimeUnrender (..), Accept (..), Capture, FromHttpApiData, parseQueryParam)
import Servant.HTML.Lucid (HTML)
import Servant.Client (ClientM, client, runClientM, mkClientEnv, Scheme(..), BaseUrl(..), ClientError (ConnectionError))
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import GHC.Conc (TVar, readTVarIO, readTVar, writeTVar, newTVarIO, newTVar, forkIO, atomically, threadDelay)

import qualified Database
import System.IO

callStopPassage :: IO ()
callStopPassage = do
  response <- getStopPassages Nothing (Left churchCrossEast)
  case response of
    Just (StopPassageResponse passages) -> do
      traverse_ (print . departure) passages
    _ -> putStrLn "Timeout while calling stopPassage"

getStopPassages :: Maybe Manager -> Either StopId TripId -> IO (Maybe StopPassageResponse)
getStopPassages Nothing stopOrTripId = do
  manager <- newManager tlsManagerSettings
  result <- runClientM (stopPassage
                        (either Just (const Nothing) stopOrTripId)
                        (either (const Nothing) Just stopOrTripId))
                        (mkClientEnv manager (BaseUrl Https "buseireann.ie" 443 ""))
  case result of
    Left e -> do
      putStrLn "Failed to get bus stop passages: "
      print e
      return Nothing
    Right r -> return (Just r)
getStopPassages (Just manager) stopOrTripId = ignoringTimeouts
  (runClientM (stopPassage
                        (either Just (const Nothing) stopOrTripId)
                        (either (const Nothing) Just stopOrTripId))
                                    (mkClientEnv manager (BaseUrl Https "buseireann.ie" 443 "")))

ignoringTimeouts :: IO (Either ClientError a) -> IO (Maybe a)
ignoringTimeouts f = do
  r <- f
  case r of
    Left (ConnectionError e) ->
      case fromException e of
        Just (HttpExceptionRequest _ ResponseTimeout) -> return Nothing
        _ -> do
          putStrLn ("ConnectionError: " <> show e)
          return Nothing
    Left e -> do
      putStrLn ("Other ClientError: " <> show e)
      return Nothing
    Right r -> return (Just r)

callStopPoints :: IO ()
callStopPoints = do
  Just (BusStopPoints stops) <- getStopPoints Nothing
  putStrLn ("Got " <> show (Vector.length stops) <> " stops.")
  putStrLn ("The first is: " <> show (stops Vector.! 0))

getStopPoints :: Maybe Manager -> IO (Maybe BusStopPoints)
getStopPoints Nothing = ignoringTimeouts do
  manager <- newManager tlsManagerSettings
  runClientM stopPoints (mkClientEnv manager (BaseUrl Https "buseireann.ie" 443 ""))
getStopPoints (Just manager) = ignoringTimeouts do
  manager <- newManager tlsManagerSettings
  runClientM stopPoints (mkClientEnv manager (BaseUrl Https "buseireann.ie" 443 ""))

makeStopPointsHtml :: IO ()
makeStopPointsHtml = do
  Just (BusStopPoints stops) <- getStopPoints Nothing
  let makeOption :: BusStop -> Html ()
      makeOption stop = option_ [value_ (name stop <> " [" <> Text.pack (show (stop ^. field @"number")) <> "]")] (pure ())
  let html = main_ $ body_ $ do
        input_ [Lucid.type_ "text", list_ "stops"]
        datalist_ [id_ "stops"] (select_ (foldMap makeOption stops))
  Lucid.renderToFile "Stops.html" html

getRoutes :: IO (Maybe Routes)
getRoutes = ignoringTimeouts do
  manager <- newManager tlsManagerSettings
  runClientM routesEndpoint (mkClientEnv manager (BaseUrl Https "buseireann.ie" 443 ""))

getRouteStopMap :: IO (Map RouteId (Set BusStop))
getRouteStopMap = do
  manager <- newManager tlsManagerSettings
  stopsResponse <- getStopPoints (Just manager)
  case stopsResponse of
    Nothing -> return Map.empty
    Just (BusStopPoints stops) -> do
      let
        getRoutes :: BusStop -> IO (Vector RouteId)
        getRoutes stop = do
          let stopId = stop ^. field @"id"
          response <- getStopPassages (Just manager) (Left stopId)
          let f :: StopPassageResponse -> Vector RouteId
              f = toVectorOf (field @"stopPassageTdi" . traverse . field @"routeId")
          return (maybe Vector.empty f response)
        getBatchRoutes :: Integer -> Vector BusStop -> IO (Vector BusStop, Vector (Vector RouteId))
        getBatchRoutes n stops = do
          putStrLn ("Starting batch " <> show n)
          let (batch, rest) = Vector.splitAt 10 stops
          batchRoutes :: Vector (Vector RouteId) <- mapConcurrently getRoutes batch
          putStrLn ("Finished batch " <> show n)
          putStrLn "Waiting…"
          threadDelay 1000000 -- 1 second
          return (rest, batchRoutes)
        getBatches :: Integer -> Vector BusStop -> IO (Vector (Vector RouteId))
        getBatches n stops = do
          (rest, routes) <- getBatchRoutes n stops
          if Vector.null rest
          then return routes
          else return routes <> getBatches (n + 1) rest
      routes <- getBatches 1 stops
      let
        pairs :: Vector (RouteId, Set BusStop)
        pairs = fmap (\(x, y) -> (y, Set.singleton x))
          (join (Vector.zipWith (\s rs -> fmap (s ,) rs) stops routes))
      return (Map.fromListWith Set.union (Vector.toList pairs))

writeRouteStopMaps :: FilePath -> IO ()
writeRouteStopMaps outputFile = do
  manager <- newManager tlsManagerSettings
  stopsResponse <- getStopPoints (Just manager)
  case stopsResponse of
    Just (BusStopPoints stops) -> do
      let
        writeRoutes :: BusStop -> IO ()
        writeRoutes stop = do
          let stopId@(StopId s) = stop ^. field @"id"
          passageResponse <- getStopPassages (Just manager) (Left stopId)
          case passageResponse of
            Just (StopPassageResponse passages) -> let
              routes = fmap (^. field @"routeId") passages
              object = Map.fromList [("routes" :: Text, toJSON routes), ("stop", toJSON stop)]
              in encodeFile ("data/routeMap/" <> Text.unpack s <> ".json") object
            Nothing -> putStrLn "Timed out getting passages!"
        writeBatchRoutes :: Integer -> Vector BusStop -> IO (Vector BusStop)
        writeBatchRoutes n stops = do
          putStrLn ("Starting batch " <> show n)
          let (batch, rest) = Vector.splitAt 10 stops
          mapConcurrently writeRoutes batch
          putStrLn ("Finished batch " <> show n)
          putStrLn "Waiting…"
          threadDelay 1000000 -- 1 second
          return rest
        writeBatches :: Integer -> Vector BusStop -> IO ()
        writeBatches n stops = do
          rest <- writeBatchRoutes n stops
          if Vector.null rest
          then return ()
          else writeBatches (n + 1) rest
      writeBatches 1 stops
    Nothing -> putStrLn "Timed out getting stops!"

churchCrossEast :: StopId
churchCrossEast = StopId "7338653551721429731"

stopPassage :: Maybe StopId -> Maybe TripId -> ClientM StopPassageResponse
stopPoints :: ClientM BusStopPoints
routesEndpoint :: ClientM Routes

stopPassage :<|> stopPoints :<|> routesEndpoint = client busEireannApi

busEireannApi :: Proxy BusEireannAPI
busEireannApi = Proxy

type BusEireannAPI = "inc" :> "proto" :> (
  ("stopPassageTdi.php"
    :> QueryParam "stop_point" StopId
    :> QueryParam "trip" TripId
    :> Get '[JSON] StopPassageResponse)
  :<|> ("bus_stop_points.php" :> Get '[StrippedJavaScript] BusStopPoints)
  :<|> ("routes.php" :> Get '[StrippedJavaScript] Routes)
  )

type BusboyAPI = "stop" :> Capture "stop_id" StopId :> Get '[HTML] StopPage

data StopPage = StopPage
  { stopId :: StopId
  , stopPageData :: RealtimeStopData
  }

instance ToHtml StopPage where
  toHtml (StopPage (StopId stopId) stopData) = main_ $ body_ $ do
    let n = Map.size stopData
    let emptyRow :: Monad m => PassageId -> HtmlT m ()
        emptyRow (PassageId passageId) = tr_ do
            td_ (toHtml passageId)
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
            td_ "-"
    let passageRow :: Monad m => Passage -> HtmlT m ()
        passageRow Passage
            { id = PassageId passageId
            , tripId = TripId tripId
            , routeId = RouteId routeId
            , vehicleId
            , patternId = Just (PatternId patternId)
            , isDeleted
            , direction
            , lastModified = MillisTimestamp lastModified
            , departure = Just (TimingData
              { scheduledPassageTime = scheduledDepartureTime
              , actualPassageTime = actualDepartureTime
              , directionText = departureDirectionText
              , serviceMode = departureServiceMode
              , type_ = departureType
              })
            , arrival = Just (TimingData
              { scheduledPassageTime = scheduledArrivalTime
              , actualPassageTime = actualArrivalTime
              , directionText = arrivalDirectionText
              , serviceMode = arrivalServiceMode
              , type_ = arrivalType
              })
            }
          = tr_ do
            td_ (toHtml passageId)
            td_ (toHtml tripId)
            td_ (toHtml routeId)
            td_ (maybe "-" (toHtml . unVehicleId) vehicleId)
            td_ (toHtml patternId)
            td_ (toHtml (Text.pack (show isDeleted)))
            td_ (toHtml (Text.pack (show direction)))
            td_ (toHtml (Text.pack (show lastModified)))
            td_ (maybe "-" (toHtml . Text.pack . show . unSecondsTimestamp) scheduledDepartureTime)
            td_ (maybe "-" (toHtml . Text.pack . show . unSecondsTimestamp) actualDepartureTime)
            td_ (toHtml departureDirectionText)
            td_ (toHtml (Text.pack (show departureServiceMode)))
            td_ (toHtml (Text.pack (show departureType)))
            td_ (maybe "-" (toHtml . Text.pack . show . unSecondsTimestamp) scheduledArrivalTime)
            td_ (maybe "-" (toHtml . Text.pack . show . unSecondsTimestamp) actualArrivalTime)
            td_ (toHtml arrivalDirectionText)
            td_ (toHtml (Text.pack (show arrivalServiceMode)))
            td_ (toHtml (Text.pack (show arrivalType)))
    p_ (toHtml (Text.pack (show n)) <> " passages found for stop " <> toHtml stopId <> ":")
    table_ $ do
      tr_ do
        th_ "Passage ID"
        th_ "Trip ID"
        th_ "Route ID"
        th_ "Vehicle ID"
        th_ "Pattern ID"
        th_ "Deleted?"
        th_ "Direction"
        th_ "Last modified"
        th_ "Scheduled departure time"
        th_ "Actual departure time"
        th_ "Direction text (departure)"
        th_ "Service mode (departure)"
        th_ "Type (departure)"
        th_ "Scheduled arrival time"
        th_ "Actual arrival time"
        th_ "Direction text (arrival)"
        th_ "Service mode (arrival)"
        th_ "Type (arrival)"
      flip foldMap (Map.assocs stopData) \(pId, v) ->
        case Vector.unsnoc v of
          Nothing -> emptyRow pId
          Just (_, (_, Left _)) -> emptyRow pId
          Just (_, (_, Right p)) -> passageRow p

  toHtmlRaw = toHtml

-- dataCollector :: IO ()
-- dataCollector = do
--   var <- newTVarIO initialState
--   forkIO (collectData var)
--   run 9999 (dataCollectorApp var)
--   where
--     initialState :: DataCollectorState
--     initialState = DataCollectorState
--       { predictions = Vector.empty
--       , locations = Vector.empty
--       , vehicles = Map.empty
--       , passages = Map.empty
--       }

data DataCollectorState = DataCollectorState
  { predictions :: Vector Prediction
  , locations :: Vector Location
  , vehicles :: Map VehicleId (Set VehicleInfo)
  , passages :: Map PassageId (Set PassageInfo)
  }

-- | Recorded when it’s changed. One per passage.
data Prediction = Prediction
  { retrievedAt :: UTCTime
  , passageId :: PassageId
  , stopId :: StopId
  , lastModified :: MillisTimestamp
  , scheduledArrivalTime :: Maybe SecondsTimestamp
  , actualOrEstimatedArrivalTime :: Maybe SecondsTimestamp
  , scheduledDepartureTime :: Maybe SecondsTimestamp
  , actualOrEstimatedDepartureTime :: Maybe SecondsTimestamp
  } deriving (Show, Eq, Generic)

instance SQLite.ToRow Prediction where
  toRow Prediction{..} =
    [ SQLite.toField retrievedAt
    , SQLText (coerce passageId)
    , SQLText (coerce stopId)
    , SQLite.toField (coerce lastModified :: UTCTime)
    , SQLite.toField (coerce scheduledArrivalTime :: Maybe UTCTime)
    , SQLite.toField (coerce actualOrEstimatedArrivalTime :: Maybe UTCTime)
    , SQLite.toField (coerce scheduledDepartureTime :: Maybe UTCTime)
    , SQLite.toField (coerce actualOrEstimatedDepartureTime :: Maybe UTCTime)
    ]

instance SQLite.FromRow Prediction where
  fromRow = Prediction
    <$> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field

-- | Recorded every time it's measured. One per vehicle.
data Location = Location
  { retrievedAt :: UTCTime
  , vehicleId :: VehicleId
  , latitude :: Integer
  , longitude :: Integer
  , bearing :: Integer
  , congestionLevel :: Maybe Integer
  , accuracyLevel :: Maybe Integer
  , lastModified :: MillisTimestamp
  } deriving (Show, Eq)

instance SQLite.ToRow Location where
  toRow Location{..} =
    [ SQLite.toField retrievedAt
    , SQLText (coerce vehicleId)
    , SQLite.toField latitude
    , SQLite.toField longitude
    , SQLite.toField bearing
    , SQLite.toField congestionLevel
    , SQLite.toField accuracyLevel
    , SQLite.toField (coerce lastModified :: UTCTime)
    ]

-- | Recorded when it changes.
data VehicleInfo = VehicleInfo
  { retrievedAt :: UTCTime
  , vehicleId :: VehicleId
  , isAccessible :: Maybe Bool
  , hasBikeRack :: Maybe Bool
  } deriving (Show, Eq, Generic, Ord)

instance SQLite.ToRow VehicleInfo where
  toRow VehicleInfo{..} =
    [ SQLite.toField retrievedAt
    , SQLText (coerce vehicleId)
    , SQLite.toField isAccessible
    , SQLite.toField hasBikeRack
    ]

instance SQLite.FromRow VehicleInfo where
  fromRow = VehicleInfo
    <$> SQLite.field
    <*> fmap (coerce :: Text -> VehicleId) SQLite.field
    <*> SQLite.field
    <*> SQLite.field

-- | Recorded when it changes.
data PassageInfo = PassageInfo
  { id :: PassageId
  , retrievedAt :: UTCTime
  , tripId :: TripId
  , routeId :: RouteId
  , stopId :: StopId
  , vehicleId :: Maybe VehicleId
  } deriving (Generic)

instance SQLite.ToRow PassageInfo where
  toRow PassageInfo{..} =
    [ SQLText (coerce id)
    , SQLite.toField retrievedAt
    , SQLText (coerce tripId)
    , SQLText (coerce routeId)
    , SQLText (coerce stopId)
    , SQLite.toField (coerce vehicleId :: Maybe Text)
    ]

timeAction :: LogAction IO Text -> Text -> IO a -> IO a
timeAction log name io = do
  before <- getCurrentTime
  r <- io
  after <- getCurrentTime
  log <& (name <> ": " <> Text.pack (show (diffUTCTime after before)))
  return r

collectData :: LogAction IO Text -> Manager -> SQLite.Connection -> IO ()
collectData log manager connection = do
  -- for each stop on the 220 route:
  -- - poll the stopPassage endpoint
  -- - save retrieved info into the state variable
  --
  -- v2:
  -- call church cross east, get a list of trip ids
  -- for each of those trips:
  --   call stopPassage
  --   store the retrieved info for each stop in the response
  churchCrossEastResponse <- liftIO (getStopPassages (Just manager) (Left churchCrossEast))
  case churchCrossEastResponse of
    Nothing -> putStrLn "Timed out querying church cross east: waiting for next step"
    Just (StopPassageResponse passages) -> do
      now <- getCurrentTime
      let retrievedAt = now
      let tripIds = fmap (^. field @"tripId") passages
      let vehicleInfo :: Set VehicleInfo = Vector.toList passages &
            mapMaybe (\Passage{..} -> do
                         vehicleId' <- vehicleId
                         return VehicleInfo
                           { vehicleId = vehicleId'
                           , isAccessible = coerce isAccessible
                           , hasBikeRack = coerce hasBikeRack
                           , retrievedAt
                           })
            & Set.fromList
      oldVehicleInfo <- timeAction log "select vehicles"
        (fmap Set.fromList (SQLite.query_ connection
          "select vehicleId, isAccessible, hasBikeRack \
          \ from vehicles \
          \ order by retrievedAt desc "
          :: IO [(Text, Maybe Bool, Maybe Bool)]))
      SQLite.withTransaction connection do
        timeAction log "insert vehicles"
          (for_ vehicleInfo \vi -> do
            unless (( vi ^. field @"vehicleId" . to coerce
                  , vi ^. field @"isAccessible"
                  , vi ^. field @"hasBikeRack"
                  ) `Set.member` oldVehicleInfo)
              (timeAction log "insert vehicles (inner)"
               (SQLite.execute connection "insert into vehicles values (?, ?, ?, ?)" vi)))
      let locations :: [Location] = Vector.toList passages &
            mapMaybe (\Passage{..} -> do
                         vehicleId' <- vehicleId
                         return Location{vehicleId = vehicleId', ..})
      timeAction log "insert locations"
        (SQLite.executeMany connection "insert into locations values (?, ?, ?, ?, ?, ?, ?, ?)" locations)
      for_ tripIds \trip -> do
        tripResponse <- liftIO (getStopPassages (Just manager) (Right trip))
        case tripResponse of
          Nothing -> liftIO (putStrLn ("Time out on trip " <> Text.unpack (unTripId trip) <> ": skipping"))
          Just (StopPassageResponse tripPassages) -> do
            now <- getCurrentTime
            let retrievedAt = now
            mostRecentPredictions :: Map (PassageId, StopId) Prediction <- fmap (Map.fromList
                                          . fmap (\p@Prediction{passageId, stopId} -> ((passageId, stopId), p)))
                                     (timeAction log "select predictions"
                                      (SQLite.query_ connection
                                       "select retrievedAt, passageId, stopId, lastModified, \
                                       \   scheduledArrivalTime, actualOrEstimatedArrivalTime, \
                                       \   scheduledDepartureTime, actualOrEstimatedDepartureTime from \
                                       \ (select *, \
                                       \   row_number() over (partition by passageId, stopId order by retrievedAt desc) ranked_order \
                                       \ from predictions) \
                                       \ where ranked_order = 1;" :: IO [Prediction]))
            let predictionsToInsert :: [Prediction] = Vector.toList tripPassages &
                  mapMaybe (\p@Passage{..} -> do
                    let passageId = id
                    (scheduledArrivalTime, actualOrEstimatedArrivalTime) <- do
                      td <- arrival
                      return (td ^. field @"scheduledPassageTime", td ^. field @"actualPassageTime")
                    (scheduledDepartureTime, actualOrEstimatedDepartureTime) <- do
                      td <- departure
                      return (td ^. field @"scheduledPassageTime", td ^. field @"actualPassageTime")
                    let prediction = Prediction{..}
                    let passageSubfields p =
                          ( p ^. field @"lastModified"
                           , p ^. field @"scheduledArrivalTime"
                           , p ^. field @"actualOrEstimatedArrivalTime"
                           , p ^. field @"scheduledDepartureTime"
                           , p ^. field @"actualOrEstimatedDepartureTime"
                           )
                    guard (maybe
                          True
                           ((passageSubfields prediction /=) . passageSubfields)
                           (Map.lookup (passageId, stopId) mostRecentPredictions))
                    return Prediction{..})
            timeAction log "insert predictions"
              (SQLite.withTransaction connection
               (SQLite.executeMany connection "insert into predictions values (?, ?, ?, ?, ?, ?, ?, ?)" predictionsToInsert))
            let passageInfo :: [PassageInfo] = Vector.toList tripPassages &
                  fmap (\Passage{..} -> PassageInfo{..})
            mostRecentPassageInfo <- timeAction log "select passages" (fmap (Map.fromList
                                           . fmap (\(a, b, c, d, e, f, g) -> (a, (b, c, d, e, f))))
                                     (SQLite.query_ connection
              -- need rest of row though
              " select * from \
              \ (select *, \
              \   row_number() over (partition by id order by retrievedAt desc) ranked_order \
              \ from passages) \
              \ where ranked_order = 1; \
              \ " :: IO [(PassageId, UTCTime, TripId, RouteId, StopId, Maybe VehicleId, Integer)]))
            timeAction log "insert passages"
              (for_ passageInfo \pi -> do
                when (maybe True
                       (\(t, tId, rId, sId, vId) ->
                          (tId, rId, sId, vId)
                            /= (pi ^. field @"tripId", pi ^. field @"routeId"
                               , pi ^. field @"stopId", pi ^. field @"vehicleId")
                          || (t < addUTCTime (negate nominalDay) (pi ^. field @"retrievedAt")))
                       (Map.lookup (pi ^. field @"id") mostRecentPassageInfo)
                     )
                  (timeAction log "insert passages (inner)"
                   (SQLite.execute connection "insert into passages values (?, ?, ?, ?, ?, ?)" pi))) -- should be an upsert?

dataCollectorApp :: TVar DataCollectorState -> Application
dataCollectorApp =
  -- show the number of records collected so far in each "table"
  -- maybe an endpoint for saving to a csv?
  undefined

getStopsForRoute :: RouteId -> IO (Maybe (Set StopId))
getStopsForRoute route = ignoringTimeouts do
  stopPoints <- getStopPoints Nothing
  let routeForStop = undefined
  undefined

busboyAPI :: Proxy BusboyAPI
busboyAPI = Proxy

busboyApp :: ServerState -> Application
busboyApp = serve busboyAPI . busboyServer

runBusboyApp :: FilePath -> FilePath -> IO ()
runBusboyApp databasePath logPath = withFile logPath AppendMode \logHandle -> do
  let log = logTextHandle logHandle <> logFlush logHandle
  forkIO (SQLite.withConnection databasePath (\connection -> do
    let managerSettings = tlsManagerSettings
          { managerModifyRequest = \r -> return (r {responseTimeout = responseTimeoutMicro 10000000}) -- 10 seconds
          }
    manager <- newManager managerSettings
    Database.createTables connection
    let loop f = do
          now <- getCurrentTime
          log <& ("Looping at " <> Text.pack (show now))
          f
          threadDelay 10000000 -- 10 seconds
          loop f
    loop (collectData log manager connection)))
  var <- newTVarIO Map.empty
  run 9998 (busboyApp (ServerState var))

queryBusEireann :: ServerState -> IO ()
queryBusEireann ServerState{ stopData } = do
  Just (StopPassageResponse passages) <- liftIO (getStopPassages Nothing (Left churchCrossEast))
  now <- getCurrentTime
  (liftIO . atomically) do
    stopDataMap <- readTVar stopData
    m <- case Map.lookup churchCrossEast stopDataMap of
      Nothing -> do
        let map = Map.fromListWith (<>) (Vector.toList (fmap (\p@Passage{id} ->
                                          (id, Vector.singleton (now, Right p))) passages))
        stopVar <- newTVar (StopData now map)
        return (Map.insert churchCrossEast stopVar stopDataMap)
      Just stopVar -> do
        StopData{lastRequested, realtimeData} <- readTVar stopVar
        let f p@Passage{id} m = let
              g = \case
                    Nothing -> Vector.singleton (now, Right p)
                    Just v -> Vector.snoc v (now, Right p)
              in Map.alter (Just . g) id m
        writeTVar stopVar (StopData now (foldr f realtimeData passages))
        return stopDataMap
    writeTVar stopData m
  threadDelay 10000000 -- 10 seconds
  queryBusEireann (ServerState stopData)

busboyServer :: ServerState -> Server BusboyAPI
busboyServer = stopEndpoint

stopEndpoint :: ServerState -> StopId -> Handler StopPage
stopEndpoint state stopId = do
  stopDataMap <- liftIO (readTVarIO (stopData state))
  case Map.lookup stopId stopDataMap of
    Nothing -> throwError err404
    Just stopDataVar -> do
      stopData <- liftIO (readTVarIO stopDataVar)
      pure (StopPage stopId (realtimeData stopData))

newtype ServerState = ServerState
  { stopData :: TVar (Map StopId (TVar StopData)) }

data StopData = StopData
  { lastRequested :: UTCTime
  , realtimeData :: RealtimeStopData
  }

type RealtimeStopData = Map PassageId (Vector (UTCTime, Either Text Passage))

data StrippedJavaScript

instance Accept StrippedJavaScript where
  contentType _ = "text/javascript"

newtype StopId = StopId Text
  deriving (Eq, Show, Ord)
  deriving newtype (FromJSON, ToJSON, SQLite.FromField)
newtype PassageId = PassageId Text
  deriving (Eq, Show, Ord)
  deriving newtype (FromJSON, SQLite.FromField)
newtype RouteId = RouteId Text
  deriving (Eq, Show, Ord)
  deriving newtype (FromJSON, ToJSON, ToJSONKey, SQLite.FromField)
newtype VehicleId = VehicleId {unVehicleId :: Text}
  deriving (Eq, Show, Ord)
  deriving newtype (FromJSON, SQLite.FromField)
newtype PatternId = PatternId Text
  deriving (Eq, Show)
  deriving newtype FromJSON
newtype Duid a = Duid { unDuid :: a } deriving (Eq, Show)

instance FromJSON a => FromJSON (Duid a) where
  parseJSON = withObject "Duid" $ \o -> do
    a <- o .: "duid"
    pure (Duid a)

newtype TripId = TripId { unTripId :: Text }
    deriving (Eq, Show, Generic)
    deriving newtype (FromJSON, SQLite.FromField)

newtype MillisTimestamp = MillisTimestamp UTCTime
  deriving (Show, Eq)
  deriving newtype (SQLite.FromField)

instance FromJSON MillisTimestamp where
  parseJSON = withScientific "MillisTimestamp" $
    \s -> case floatingOrInteger s :: Either Double Integer of
      Left f ->
        fail $ "parsing MillisTimestamp failed, expected integer value but got: "
          <> show f
      Right i ->
        pure (MillisTimestamp (posixSecondsToUTCTime
          (secondsToNominalDiffTime (MkFixed (1000000000 * i)))))

newtype SecondsTimestamp = SecondsTimestamp {unSecondsTimestamp :: UTCTime}
  deriving (Show, Eq, Ord)
  deriving newtype (SQLite.FromField)

instance FromJSON SecondsTimestamp where
  parseJSON = withScientific "SecondsTimestamp" $
    \s -> case floatingOrInteger s :: Either Double Integer of
      Left f ->
        fail $ "parsing SecondsTimestamp failed, expected integer value but got: "
          <> show f
      Right i ->
        pure (SecondsTimestamp (posixSecondsToUTCTime (fromIntegral i)))

instance ToHttpApiData StopId where
  toQueryParam (StopId t) = t

instance FromHttpApiData StopId where
  parseQueryParam t =
    if Text.all isDigit t
    then Right (StopId t)
    else Left "Failed to parse StopId – found non-digit"

instance ToHttpApiData TripId where
  toQueryParam (TripId t) = t

instance FromHttpApiData TripId where
  parseQueryParam t =
    if Text.all isDigit t
    then Right (TripId t)
    else Left "Failed to parse TripId – found non-digit"

newtype StopPassageResponse = StopPassageResponse
  { stopPassageTdi :: Vector Passage
  } deriving (Show, Generic)

instance FromJSON StopPassageResponse where
  parseJSON = withObject "StopPassageResponse" $ \js -> let
    indices = [0..] :: [Integer]
    properties = fmap (fromString . ("passage_" <>) . show) indices
    f :: (Aeson.Key -> Parser (Maybe Passage)) -> Aeson.Key -> Parser [Passage] -> Parser [Passage]
    f lookup prop parser = do
      passage <- lookup prop :: Parser (Maybe Passage)
      case passage of
        Just p -> (p :) <$> parser
        Nothing -> pure []
    in do
      o <- js .: "stopPassageTdi"
      passages <- foldr (f (o .:?)) (pure []) properties
      pure (StopPassageResponse (Vector.fromList passages))

data Passage = Passage
  { id :: PassageId
  , tripId :: TripId
  , routeId :: RouteId
  , stopId :: StopId
  , vehicleId :: Maybe VehicleId
  , patternId :: Maybe PatternId
  , isDeleted :: Bool
  , direction :: Integer
  , lastModified :: MillisTimestamp
  , arrival :: Maybe TimingData
  , departure :: Maybe TimingData
  , congestionLevel :: Maybe Integer
  , accuracyLevel :: Maybe Integer
  , status :: Integer
  , isAccessible :: Maybe ZeroOrOne
  , bearing :: Integer
  , hasBikeRack :: Maybe ZeroOrOne
  , category :: Maybe Integer
  , longitude :: Integer
  , latitude :: Integer
  -- TODO: longitude and latitude
  } deriving (Show, Generic)

instance FromJSON Passage where
  parseJSON = withObject "Passage" $ \js -> do
    i <- js .: "duid"
    isDeleted <- js .: "is_deleted"
    direction <- js .: "direction"
    lastModified <- js .: "last_modification_timestamp"
    (Duid tripId) <- js .: "trip_duid"
    (Duid routeId) <- js .: "route_duid"
    (Duid stopId) <- js .: "stop_point_duid"
    vehicleId <- (fmap . fmap) unDuid (js .:? "vehicle_duid")
    patternId <- (fmap . fmap) unDuid (js .:? "pattern_duid")
    arrival <- js .:? "arrival_data"
    departure <- js .:? "departure_data"
    congestionLevel <- js .:? "congestion_level"
    accuracyLevel <- js .:? "accuracy_level"
    status <- js .: "status"
    isAccessible <- js .:? "is_accessible"
    bearing <- js .: "bearing"
    hasBikeRack <- js .:? "has_bike_rack"
    category <- js .:? "category"
    longitude <- js .: "longitude"
    latitude <- js .: "latitude"
    pure (Passage { Lib.id = i
                  , isDeleted
                  , direction
                  , lastModified
                  , tripId
                  , routeId
                  , stopId
                  , vehicleId
                  , patternId
                  , arrival
                  , departure
                  , congestionLevel
                  , accuracyLevel
                  , status
                  , isAccessible
                  , bearing
                  , hasBikeRack
                  , category
                  , longitude
                  , latitude
                  })

data TimingData = TimingData
  { scheduledPassageTime :: Maybe SecondsTimestamp
  , actualPassageTime :: Maybe SecondsTimestamp
  , directionText :: Text
  , serviceMode :: Integer
  , type_ :: Integer
  } deriving (Show, Eq, Generic)

instance FromJSON TimingData where
  parseJSON = withObject "TimingData" $ \o -> do
    scheduledPassageTime <- o .:? "scheduled_passage_time_utc"
    actualPassageTime <- o .:? "actual_passage_time_utc"
    directionObject <- o .: "multilingual_direction_text"
    directionText <- directionObject .: "defaultValue"
    serviceMode <- o .: "service_mode"
    type_ <- o .: "type"
    pure (TimingData {..})

newtype ZeroOrOne = ZeroOrOne Bool deriving (Show, Eq)

instance FromJSON ZeroOrOne where
  parseJSON v = do
    n <- parseJSON v :: Parser Integer
    case n of
      0 -> pure (ZeroOrOne False)
      1 -> pure (ZeroOrOne True)
      _ -> fail ("Got an unexpected number (" <> show n <> "), expected 0 or 1")

newtype BusStopPoints = BusStopPoints
  { busStopPoints :: Vector BusStop
  } deriving Show

instance FromJSON BusStopPoints where
  parseJSON = withObject "BusStopPoints" \o -> do
    stops <- o .: "bus_stops"
    let keys = (\n -> fromString ("bus_stop_" <> show n)) <$> [0..]
    let getStops (k : ks) = do
          stop <- (stops .:? k) :: Parser (Maybe BusStop)
          case stop of
            Just s -> (s :) <$> getStops ks
            Nothing -> pure []
    stops <- getStops keys
    pure (BusStopPoints (Vector.fromList stops))

instance MimeUnrender StrippedJavaScript BusStopPoints where
  mimeUnrender _ byteString = let
    jsonText = decodeUtf8 byteString
    in mimeUnrender (Proxy :: Proxy JSON)
      (encodeUtf8 (LazyText.takeWhile (/= ';') (LazyText.dropWhile (/= '{') jsonText)))

data BusStop = BusStop
  { id :: StopId
  , name :: Text
  , latitude :: Double
  , longitude :: Double
  , number :: Integer
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON BusStop where
  parseJSON = withObject "BusStop" \o -> do
    id <- o .: "duid"
    name <- o .: "name"
    latitude <- o .: "lat"
    longitude <- o .: "lng"
    number <- o .: "num"
    pure (BusStop {..})

instance ToJSON BusStop

newtype Routes = Routes
  { routes :: Vector Route
  } deriving Show

instance FromJSON Routes where
  parseJSON = withObject "Routes" \o -> do
    routes <- o .: "routeTdi"
    let keys = (\n -> fromString ("routes_" <> show n)) <$> [0..]
    let getRoutes (k : ks) = do
          route <- (routes .:? k) :: Parser (Maybe Route)
          case route of
            Just s -> (s :) <$> getRoutes ks
            Nothing -> pure []
    routes <- getRoutes keys
    pure (Routes (Vector.fromList routes))

instance MimeUnrender StrippedJavaScript Routes where
  mimeUnrender _ byteString = let
    jsonText = decodeUtf8 byteString
    in mimeUnrender (Proxy :: Proxy JSON)
      (encodeUtf8 (LazyText.takeWhile (/= ';') (LazyText.dropWhile (/= '{') jsonText)))

data Route = Route
  { id :: RouteId
  , shortName :: Text
  , number :: Integer
  , direction :: Integer
  } deriving Show

instance FromJSON Route where
  parseJSON = withObject "Route" \o -> do
    id <- o .: "duid"
    shortName <- o .: "short_name"
    number <- o .: "num"
    direction <- o .: "direction_extensions" >>= (.: "direction")
    pure (Route {..})
