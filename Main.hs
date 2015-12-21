{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Array
import           Data.Function
import           Data.List                 (find, group, intercalate, maximumBy,
                                            minimumBy, nub, sort, sortBy,
                                            splitAt, zipWith4, zipWith5, (\\))
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Text.Parsec.Prim

import           GHC.IO.Handle
import           System.Directory
import           System.IO
import           Text.Printf

import           Foreign.C
import           System.CPUTime
import           System.Environment
import           System.FilePath.Posix

import           System.Log.Formatter
import           System.Log.Handler (close, setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

import           System.Random

import           Debug.Trace

import           CPLEX
import           CPLEX.Param

import           AP
import           CFLP
import           MIP

catchOutput :: IO a -> IO (a, String)
catchOutput f = do
  tmpd <- getTemporaryDirectory
  !(tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  !stdout_dup <- hDuplicate stdout
  hDuplicateTo tmph stdout
  hClose tmph
  !result <- f
  hDuplicateTo stdout_dup stdout
  hClose stdout_dup
  !str <- readFile tmpf
  removeFile tmpf
  return (result, str)

usage :: IO ()
usage = putStrLn "write n m filename|read filename|read-mip filename|run n m|bench n m"

writeCFLP :: Int -> Int -> FilePath -> IO ()
writeCFLP n m fileName = do
  cflp <- getFeasibleRandomCFLP n m
  writeFile fileName (showCFLP'' cflp)

readCFLP :: FilePath -> IO ()
readCFLP fileName = do
  cflp <- readFile fileName
  let cflp' = parse cflpFile "cflp" cflp
  case cflp' of
    Left msg -> print msg
    Right cflp'' ->
      if not $ isFeasible cflp''
      then error "CFLP not feasible"
      else do (obj, cflp''') <- solApprox cflp''
              putStrLn $ "Objective: " ++ show obj
              putStrLn $ showCFLPSolution cflp'''
              return ()

readMip :: FilePath -> IO ()
readMip fileName = do
  cflp <- readFile fileName
  let cflp' = parse cflpFile "cflp" cflp
  case cflp' of
    Left msg -> print msg
    Right cflp'' ->
      if not $ isFeasible cflp''
      then error "CFLP not feasible"
      else do (obj, sol) <- solExact cflp''
              putStrLn $ "Objective: " ++ show obj
              writeSol sol

runCFLP :: Int -> Int -> IO ()
runCFLP n m = do
  cflp <- getFeasibleRandomCFLP n m
  (obj, cflp') <- solApprox cflp
--  plotCFLP cflp' "solved.png"
  writeSol cflp'

chooseLogPoints :: Int -> Int -> [Int]
chooseLogPoints n' k' = nub [ round $ exp x | x <- [0,log n/k..log n]]
  where n = fromIntegral n'
        k = fromIntegral k'

choosePoints :: Int -> Int -> [Int]
choosePoints n' k' = map round [1,n/k..n]
  where n = fromIntegral n'
        k = fromIntegral k'

benchCFLP' :: String -> [((Int, Int), Int)]
           -> IO [(Int, Int, Int, Double, Double, Double)]
benchCFLP' testCase points = do
  liftM concat $
    forM (zip [1..] points) $ \(id, ((n,m), s)) -> do
      cflp <- case testCase of
        "uniform1" -> randomEvenDistCFLP n m
        "uniform2" -> randomEvenDist2CFLP n m
        _ -> randomEvenDistCFLP n m

      if (isFeasible cflp) then
        -- repeat for exact time measurements
        forM [1..s] $ \s -> do
          -- Exact
          ((exactObj, exactSol), exactTime) <- bench' $ solExact cflp

          -- Approx
          ((approxObj, approxSol), approxTime) <- bench' $ solApprox cflp

          return (id, n, m, exactTime, approxTime, (approxObj/exactObj))
      else return []


benchCFLP :: String -> Int -> Int -> Int -> Int
          -> IO [(Int, Int, Int, Double, Double, Double)]
benchCFLP testCase n m k r = do
  -- Update logger for no output
  updateGlobalLogger "cflp" (setLevel ERROR)

  let points = [((n,m),r) | i <- choosePoints n k, j <- choosePoints m k, j <= i]
  benchData <- benchCFLP' testCase points

  -- print data
  putStrLn "id,n,m,exactTime,approxTime,ratio"
  forM_ benchData $ \(r, n, m, exactTime, approxTime, ratio) -> do
    -- n, m, exactTime, approxTime, ratio
    putStrLn (printf "%d,%d,%d,%.8f,%.8f,%.8f"
              r n m
              exactTime approxTime ratio)
    when (abs (ratio - 1.0) > 1.0**(-8)) $ do
      putStrLn "ERROR: Ratio < 1.0"

  writeFile "bench.csv" $ show benchData
  return benchData

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

genData :: Int -> Int -> IO ()
genData n m = do
  cflp <- getFeasibleRandomCFLP n m
  ((exactObj, exactSol), exactTime) <- bench' $ solExact cflp
  ((approxObj, approxSol), approxTime) <- bench'$ solApprox cflp

  currentTime <- getCurrentTime
  let dirName = iso8601 currentTime

  createDirectory dirName
  writeFile (dirName </> "problem.cflp") $ show cflp
  writeFile (dirName </> "exact.sol") $ showCFLPSolution exactSol
  writeFile (dirName </> "approx.sol") $ showCFLPSolution approxSol
  writeFile (dirName </> "stat.txt")
    $ printf ("exact: %.15f, approx: %.15f, ratio: %.15f, "
              ++ "exactTime: %.15f, approxTime: %.15f\n")
        exactObj approxObj (approxObj/exactObj) exactTime approxTime

--  plotCFLP cflp $ (dirName </> "problem.png")
--  plotCFLP exactSol $ (dirName </> "exact.png")
--  plotCFLP approxSol $ (dirName </> "approx.png")

  return ()

mean :: [Double] -> Double
mean list = (sum list) / (fromIntegral $ length list)

genBench :: FilePath -> String -> Double -> Int -> Int -> IO [(Int, Int, Int, Double, Double, Double)]
genBench fileName testCase maxDuration stepSize numReps = do
  -- Update logger for no output
  updateGlobalLogger "cflp" (setLevel ERROR)

  -- numReps is the number of different test instances
  -- the 1 in (2*i, i), 1 is how often each instance should be run
  let points = concatMap
                 (replicate numReps)
                 [((2*i, i), 1) | i <- [stepSize,2*stepSize..]]

  let mipTime (_,_,_,t,_,_) = t
--  untilM mipTime
  let loop tests acc = do
        r <- benchCFLP' testCase $ take numReps tests
        putStrLn $ show (fst . fst . head $ tests) ++ ": " ++ show (mean (map mipTime r))
        case r of (r' : _) ->
                    if mean (map mipTime r) < maxDuration
                    then loop (drop numReps tests) (acc ++ r)
                    else return $ acc ++ r -- do not throw away tes points
                  [] -> loop (drop numReps tests) acc -- cut out infeasibles
  benchData <- loop points []

  -- print data
  putStrLn "id,n,m,exactTime,approxTime,ratio"
  forM_ benchData $ \(r, n, m, exactTime, approxTime, ratio) -> do
    -- n, m, exactTime, approxTime, ratio
    putStrLn (printf "%d,%d,%d,%.8f,%.8f,%.8f"
              r n m
              exactTime approxTime ratio)
    when (abs (ratio - 1.0) > 1.0**(-8)) $ do
      putStrLn "ERROR: Ratio < 1.0"

  writeFile fileName $ show benchData
  return benchData

bench' :: IO a -> IO (a, Double)
bench' action = do
  start <- getCPUTime
  v <- action
  end <- getCPUTime
  let diff = fromIntegral $ end - start
  return (v, diff/10^12)

writeSol :: CFLP -> IO ()
writeSol cflp = do
  putStrLn "Solution:"
  putStrLn (showCFLPSolution cflp)

main :: IO ()
main = do
  -- Set up logger
  h <- fileHandler "cflp.log" DEBUG
       >>= \lh -> return $
                  setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (addHandler h)
  updateGlobalLogger "cflp" (setLevel INFO)
  args <- getArgs
  case args of
    ("write"     : n' : m' : fileName : [])
      -> do let n = read n'
                m = read m'
            writeCFLP n m fileName
    ("read"      : fileName : [])
      -> readCFLP fileName
    ("read-mip"  : fileName : [])
      -> readMip fileName
    ("run"       : n' : m' : [])
      -> do let n = read n'
                m = read m'
            runCFLP n m
    ("gen-data"  : n' : m' : [])
      -> do let n = read n'
                m = read m'
            genData n m
    ("gen-bench" : fileName : testCase : maxDuration' : stepSize' : numReps' : [])
      -> do let maxDuration = read maxDuration'
                stepSize = read stepSize'
                numReps = read numReps'
            void $ genBench fileName testCase maxDuration stepSize numReps
    ("bench"     : testCase : n' : m' : k' : r' : [])
      -> do let n = read n'
                m = read m'
                k = read k'
                r = read r'
            void $ benchCFLP testCase n m k r
    _ -> usage

  -- Close logger
  close h
  return ()

-- | Adapted from http://stackoverflow.com/questions/8901252/2d-array-in-haskell
showTable arr =
  unlines $ map (unwords . map (printf "%.2f" . (arr !))) indices
  where indices = [[(x, y) | y <- [startY..endY]] | x <- [startX..endX]]
        ((startX, startY), (endX, endY)) = bounds arr

showAmat :: [(Row, Col, Double)] -> String
showAmat amat = showTable $ fromAmat amat

fromAmat :: [(Row, Col, Double)] -> Array (Int, Int) Double
fromAmat amat = array ((0,0), (n,m)) [((i,j),0.0) | i<-[0..n], j<-[0..m]] // a
  where
    n = maximum $ map (\(Row i, _, _) -> i) amat
    m = maximum $ map (\(_, Col j, _) -> j) amat
    a = map (\(Row i, Col j, x) -> ((i, j), x)) amat

randomPosition :: (Double, Double) -> (Double, Double) -> IO Position
randomPosition xRange yRange =
  do p1 <- newStdGen
     p2 <- newStdGen
     let (xPos, _) = randomR xRange p1
         (yPos, _) = randomR yRange p2
     return $ Position xPos yPos

randomFacilities :: Int -> (Double, Double) -> (Double, Double) -> (Position, Position) -> IO Facilities
randomFacilities n costRange capRange posRange =
  do g <- newStdGen
     h <- newStdGen
     let f = take n (randomRs costRange g)
         u = take n (randomRs capRange h)
     poss <- replicateM n (randomPosition (xRange posRange) (yRange posRange))
     return $ zipWith5 Facility [0,1..] f u [0.0, 0.0..] poss

randomClients :: Int -> (Double, Double) -> (Position, Position) -> IO Clients
randomClients m range posRange =
  do g <- newStdGen
     let d = take m $ randomRs range g
     poss <- replicateM m (randomPosition (xRange posRange) (yRange posRange))
     return $ zipWith3 Client [0..] d poss

randomDistances :: Int -> Int -> (Double, Double) -> IO Distances
randomDistances n m range =
  do g <- newStdGen
     h <- newStdGen
     let f = zip [0,1..] $ take n $ uncurry zip $ splitAt n (randomRs range g)
     let c = zip [0,1..] $ take m $ uncurry zip $ splitAt m (randomRs range h)
     let dist cx fx cy fy = sqrt ((cx-fx)**2 + (cy-fy)**2)
     return $ array ((0,0),(n-1,m-1)) [ ((i, j), Distance i j (dist cx fx cy fy) 0.0)
                                      | (i, (fx,fy)) <- f
                                      , (j, (cx,cy)) <- c]

getFeasibleCFLP gen =
  do cflp <- gen
     if isFeasible cflp
       then return cflp
       else getFeasibleCFLP gen

randomEvenDistCFLP :: Int -> Int -> IO CFLP
randomEvenDistCFLP n m =
  do fs <- randomFacilities n (0.0, 100.0) (0.0, 100.0) (Position 0.0 0.0, Position 100.0 100.0)
     cs <- randomClients m (0.0, 100.0) (Position 0.0 0.0, Position 100.0 100.0)
     return $ CFLP fs cs (locationDistances fs cs)

-- Good for solutions where opened facilities are not necessarily the closest ones
randomEvenDist2CFLP :: Int -> Int -> IO CFLP
randomEvenDist2CFLP n m =
  do fs <- randomFacilities n (0.0, 100000.0) (0.0, 100.0) (Position 0.0 0.0, Position 100.0 100.0)
     cs <- randomClients m (50.0, 100.0) (Position 0.0 0.0, Position 100.0 100.0)
     return $ CFLP fs cs (locationDistances fs cs)

getFeasibleRandomCFLP n m = getFeasibleCFLP $ randomEvenDist2CFLP n m


openFacility :: Facility -> Double -> Facility
openFacility f y = f { y = y }

getSol sol f = sol VS.! facilityId f

fromCpxSolution :: CFLP -> CpxSolution -> CFLP
fromCpxSolution cflp sol = cflp { facilities = openFacilities (facilities cflp) ys
                                , distances = satisfyDemand cflp xs
                                }
  where n = length $ facilities cflp
        ys = take n $ VS.toList (solX sol)
        xs = drop n $ VS.toList (solX sol)

getBudget :: CFLP -> CpxSolution -> [Double]
getBudget cflp sol = take n $ VS.toList (solPi sol)
  where n = length $ clients cflp

openFacilities :: Facilities -> [Double] -> Facilities
openFacilities = zipWith openFacility

satisfy :: Distance -> Double -> Distance
satisfy d x = d { x = x }

inc = (+) 1

satisfyDemand :: CFLP -> [Double] -> Distances
satisfyDemand mcf xs = ds // (map (\ ((i, j), Distance i' j' c x)
                                  -> ((i, j), Distance i' j' c (xs' V.! (mcfXIdx n m (Map.findWithDefault 0 i fIds') j))))
                              [ ((i,j), ds!(i,j)) | i <- fIds, j <- cIds ])
  where ds = distances mcf
        fIds = map facilityId $ facilities mcf
        cIds = map clientId $ clients mcf
        fIds' = Map.fromList $ zip fIds [0..]
        n = length $ facilities mcf --inc . fst . snd $ bounds ds
        m = length $ clients mcf --inc . snd . snd $ bounds ds
        xs' = V.fromList xs

satisfyDemand' :: CFLP -> [Double] -> Distances
satisfyDemand' mcf xs =
  let
    -- old  array elements
    ds = distances mcf

    n = length $ facilities mcf
    m = length $ clients mcf

    -- get facility ids
    fIds = map facilityId $ facilities mcf

    -- get client ids
    cIds = map clientId $ clients mcf

    -- mapping from facilityIds to array index
    fIds' = Map.fromList $ zip fIds [0..]

    -- get distances from open facilities
    as = [ ((i,j), ds!(i,j)) | i <- fIds, j <- cIds ]

    -- update fn
    updatedX :: Int -> Int -> Double
    updatedX i j = xs !! (mcfXIdx n m (fIds' Map.! i) j)

    -- make updated array elements
    ds' = map (\ ((i, j), dist) -> ((i, j), dist { x = updatedX i j })) as
    in ds // ds'



assignFacilitiesMCF :: CFLP -> CpxSolution -> CFLP
assignFacilitiesMCF mcf sol = mcf { distances = satisfyDemand mcf (VS.toList (solX sol)) }

-- Clustering

data Cluster = Cluster { clusterCenter   :: ClientId
                       , clusterElements :: [FacilityId]
                       }

instance Show Cluster where
  show (Cluster c es) = (show c) ++ ": " ++ (unwords $ map show es)

-- Step C1

showClusters cs = unlines $ map show $ sortBy (compare `on` clusterCenter) cs
printClusters cs = putStrLn $ showClusters cs

initialClusters :: [Cluster]
initialClusters = []

initialPossibleCenters :: CFLP -> [ClientId]
initialPossibleCenters = map clientId . clients

chooseNextCenter :: [ClientId] -> [Double] -> ClientId
chooseNextCenter possibleCenters budget = snd $ minimum $ filter (\a -> snd a `elem` possibleCenters) $ zip budget [0,1..]

formCluster :: [Cluster] -> CFLP -> [ClientId] -> [Double] -> Cluster
formCluster cluster cflp possibleCenters budget =
  let j = chooseNextCenter possibleCenters budget
  in Cluster j (calculateBj cluster cflp j)

isNearer :: Double -> [Double] -> Bool
x `isNearer` [] = True
x `isNearer` xs = x <= minimum xs

maybeToGuard :: Maybe Bool -> Bool
maybeToGuard Nothing  = True
maybeToGuard (Just b) = b

calculateBj :: [Cluster] -> CFLP -> ClientId -> [FacilityId]
calculateBj cluster cflp j = bj
  where fs = facilities cflp
        fj = [i | (Facility i _ _ _ _) <- fs, let xi = head $ getXs (distances cflp) [i] [j], xi > 0.0]
        nk = concatMap (\(Cluster k nk) -> nk) cluster
        ks = map clusterCenter cluster
        ds = distances cflp
        bj = [i | i <- fj
                , i `notElem` nk
                , maybeToGuard $ isNearer <$> getDistanceById ds i j <*> sequence [getDistanceById ds i k | k <- ks]]

getXs :: Distances -> [FacilityId] -> [ClientId] -> [Double]
getXs ds is js = [ x $ ds!(i,j) | i <- is, j <- js]

-- map x $ filter (\(Distance i j _ _) -> i `elem` is && j `elem` js) ds


getPossibleCenters :: CFLP -> [Cluster] -> [ClientId]
getPossibleCenters cflp currentClusters = do
  let unclustered = map clientId (clients cflp) \\ map clusterCenter currentClusters
  j <- unclustered
  let bj = calculateBj currentClusters cflp j
      xs = getXs (distances cflp) bj [j]
  guard $ sum xs >= 0.5
  return j


-- While there are possible center, form cluster around the center
c1 :: CFLP -> CpxSolution -> [Cluster] -> [ClientId] -> [Cluster]
c1 cflp sol c [] = c
c1 cflp sol c s  = formCluster c cflp s (getBudget cflp sol) : c


-- Step C2

facilityDistances :: CFLP -> FacilityId -> [Distance]
facilityDistances cflp i = [ ds!(i,j) | j <- [0..length (clients cflp) - 1] ]
  where ds = distances cflp
--  filter (\(Distance r s _ _) -> i == r) (distances cflp)

nearestClient :: CFLP -> FacilityId -> ClientId
nearestClient cflp i = j $ minimumBy (compare `on` c) dists
  where dists = facilityDistances cflp i


updateCluster' ::  [Cluster] -> (FacilityId, ClientId) -> [Cluster]
updateCluster' [] (i, j)                              = [Cluster j [i]]
updateCluster' (Cluster k fs : cs) (i, j) | j == k    = Cluster j (i : fs) : cs
                                          | otherwise = Cluster k fs : updateCluster' cs (i, j)

updateCluster :: [(FacilityId, ClientId)] -> [Cluster] -> [Cluster]
updateCluster as cs = foldl updateCluster' cs as

-- Assign remaining facilities to closest cluster center
c2 :: CFLP -> [Cluster] -> [Cluster]
c2 cflp clusters = updateCluster facilityAssignment clusters
  where openFacilities = [i | (Facility i _ _ yi _) <- facilities cflp, yi > 0.0]
        clusteredFacilities = concatMap clusterElements clusters
        remainingFacilities = openFacilities \\ clusteredFacilities
        facilityAssignment = zip remainingFacilities $ map (nearestClient cflp) remainingFacilities


traceMsgId :: Show a => String -> a -> a
traceMsgId msg val = val -- trace (msg ++ show val) val


-- Reducing to single node

type SNFacilityId = Int

data SNFacility = SNFacility { snFacilityId  :: SNFacilityId
                             , snOpeningCost :: Double
                             , snCapacity    :: Double
                             , snDistance    :: Double
                             , snDemand      :: Double
                             } deriving (Show)

data SNCFLP = SNCFLP { snFacilities  :: [SNFacility]
                     , snTotalDemand :: Double
                     }

instance Show SNCFLP where
  show (SNCFLP fs d) = "D: " ++ show d ++ "\n" ++ showSnFacilities fs

showSnFacilities :: [SNFacility] -> String
showSnFacilities fs = "i: " ++ unwords (map (printf "%d" . snFacilityId) fs) ++ "\n"
                      ++ showFormat "f: " snOpeningCost fs ++ "\n"
                      ++ showFormat "u: " snCapacity fs ++ "\n"
                      ++ showFormat "c: " snDistance fs ++ "\n"
                      ++ showFormat "x: " snDemand fs


clusterToSNCFLP :: CFLP -> Cluster -> Maybe (ClientId, SNCFLP)
clusterToSNCFLP cflp (Cluster k nk) = if null snfs then Nothing
                                      else Just (k, sncflp)
  where fs = mapMaybe (findFacility (facilities cflp)) nk
        cs = map clientId (clients cflp)
        ds = distances cflp
        lk = filter (\i -> y i < 1.0) fs

        snfids = map facilityId lk
        snocs  = map f lk
        sncs   = map u lk
        snds   = mapMaybe (\i -> getDistanceById ds i k) snfids

        totalDemand = sum $ getXs ds snfids cs
        snfs        = zipWith5 SNFacility snfids snocs sncs snds [0.0,0.0..]

        sncflp = SNCFLP snfs totalDemand

-- TODO: we need to actually bring back the update to the snfacility also to the cflp facility
updateSNFacility :: SNFacility -> Double -> SNFacility
updateSNFacility f x = f { snDemand = x }

updateSNCFLP :: SNCFLP -> [Double] -> SNCFLP
updateSNCFLP (SNCFLP fs d) vs = SNCFLP (zipWith updateSNFacility fs vs) d

-- TODO: Test if the right values are used (vs = zipWith (/) ... us) vs. (vs = zipWith (/) ... ws) etc.
solveSNCFLP :: SNCFLP -> SNCFLP
solveSNCFLP sncflp = updateSNCFLP sncflp vs''
  where fs = map snOpeningCost $ snFacilities sncflp
        us = map snCapacity $ snFacilities sncflp
        cs = map snDistance $ snFacilities sncflp
        ws = zipWith3 (\f u c -> f/u + c) fs us cs
        (order, ws') = unzip $ sortBy (compare `on` snd) $ zip [1..] ws
        vs = zipWith (/) (greedySolve ws $ snTotalDemand sncflp) us
        vs' = map (\x -> if x > 0.0 then 1.0 else 0.0) vs
        (order', vs'') = unzip $ sortBy (compare `on` fst) $ zip order vs'

greedySolve :: [Double] -> Double -> [Double]
greedySolve []       d = []
greedySolve vs       0 = replicate (length vs) 0.0
greedySolve (v : vs) d | d < v  = d : greedySolve vs 0.0
                       | d >= v = v : greedySolve vs (d - v)

getOpenedFacilitiesFromClusters :: CFLP -> [Cluster] -> Facilities
getOpenedFacilitiesFromClusters cflp clusters = filter (\i -> y i > 0.0) fs
  where nk = concatMap clusterElements clusters
        fs = mapMaybe (findFacility (facilities cflp)) nk

getOpenedFacilitiesFromSNCFLPs :: CFLP -> [SNCFLP] -> Facilities
getOpenedFacilitiesFromSNCFLPs cflp sncflps = fs
  where nk = filter (\f -> snDemand f > 0.0 ) $ concatMap snFacilities sncflps
        fs = mapMaybe (findFacility (facilities cflp) . snFacilityId) nk


updateFacilities :: Facilities -> Facilities -> Facilities
updateFacilities orig new = map update orig
  where update f =
          case findFacility new (facilityId f) of
            Just f' -> f'
            Nothing -> f


solExact :: CFLP -> IO (Double, CFLP)
solExact cflp =
  case fromCFLP cflp of
    Nothing -> error "Could not create MIP"
    Just mip -> do
        infoM "cflp" "Solving mixed integer program"
        (sol, stdout) <- catchOutput $ solMip "MIP" mip
--        print $ length stdout
        return $ (solObj sol, fromCpxSolution cflp sol)

-- Assign clients

solApprox :: CFLP -> IO (Double, CFLP)
solApprox cflp = do
  infoM "cflp" "Solving relaxed linear program"
  (lpSol, stdout) <- catchOutput $ solLp "CFLP" $ fromJust . fromCFLP $ cflp
  let relaxedCFLP = fromCpxSolution cflp lpSol
  infoM "cflp" "Relaxed CFLP:"
  infoM "cflp" $ show relaxedCFLP

  infoM "cflp" "Clustering facilities"
  let cs  = c1 relaxedCFLP lpSol [] (getPossibleCenters relaxedCFLP [])
      cs' = c2 relaxedCFLP cs
  infoM "cflp" "C1:"
  infoM "cflp" $ show cs
  infoM "cflp" "C2:"
  infoM "cflp" $ show cs'


  infoM "cflp" "Solving SNCFLPs"
  let sncflps  = mapMaybe (clusterToSNCFLP relaxedCFLP) cs'
      sncflps' = map (solveSNCFLP . snd) sncflps
  infoM "cflp" "SNCFLPs:"
  infoM "cflp" $ show sncflps'

  infoM "cflp" "Collecting opened facilities"
  let openedFs   = getOpenedFacilitiesFromClusters relaxedCFLP cs'
      openedFs'  = getOpenedFacilitiesFromSNCFLPs relaxedCFLP sncflps'
      openedFs'' = openedFs ++ openedFs'
      openedIds  = map facilityId openedFs''
  infoM "cflp" "Open facilities:"
  infoM "cflp" $ show openedIds

  let openedCFLP = CFLP (map (\f -> f { y = 1.0 })
                         (filter (\f -> facilityId f `elem` openedIds)
                          (facilities cflp)))
                        (clients cflp)
                        (distances cflp)
  infoM "cflp" "Open CFLP:"
  infoM "cflp" $ show openedCFLP

  let mcf = fromOpenedCFLP openedCFLP
  infoM "cflp" "MCF:"
  infoM "cflp" $ show mcf

  infoM "cflp" "Solving assignment problem"
  (mcfSol, stdout) <- catchOutput $ solLp "MCF" mcf
  let openedMcf = assignFacilitiesMCF openedCFLP mcfSol
      openedObj = (solObj mcfSol) + sum (map f (facilities openedCFLP))
  infoM "cflp" "Open MCF:"
  infoM "cflp" $ show openedMcf

  infoM "cflp" "Setting solution into original problem"
  let solvedCFLP = CFLP (updateFacilities
                         (facilities cflp)
                         (facilities openedMcf))
                        (clients openedMcf)
                        (distances openedMcf)


  return (openedObj, solvedCFLP)
