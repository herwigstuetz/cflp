{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Array
import           Data.Function
import           Data.List                 (find, group, intercalate, maximumBy,
                                            minimumBy, sort, sortBy, splitAt,
                                            zipWith4, zipWith5, (\\))
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import qualified Data.Set                  as Set
import qualified Data.Vector               as V
import qualified Data.Vector.Storable      as VS

import           Data.Array
import           GHC.IO.Handle
import           System.Directory
import           System.IO
import           Text.Printf

import           Foreign.C
import           System.Random

import           Debug.Trace

import           CPLEX
import           CPLEX.Param

import           AP
import           CFLP
import           MIP

catchOutput :: IO () -> IO String
catchOutput f = do
  tmpd <- getTemporaryDirectory
  (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  stdout_dup <- hDuplicate stdout
  hDuplicateTo tmph stdout
  hClose tmph
  f
  hDuplicateTo stdout_dup stdout
  str <- readFile tmpf
  removeFile tmpf
  return str

main :: IO ()
main = do
  cflp <- randomCFLP 4 4

  if not $ isFeasible cflp
    then main
    else case fromCFLP cflp of Nothing -> main
                               Just mip' -> do
                                 solMip "CFLP" mip'
                                 sol cflp

-- | Adapted from http://stackoverflow.com/questions/8901252/2d-array-in-haskell
showTable arr =
  unlines $ map (unwords . map ((printf "%.2f") . (arr !))) indices
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


type IdManagement = State Int

generateId :: IdManagement Int
generateId = do
  n <- get
  put (n + 1)
  return n

createFacility :: (Double, Double) -> IdManagement Facility
createFacility (f, u) = do
  id <- generateId
  return $ Facility id f u 0.0

createClient :: Double -> IdManagement Client
createClient d = do
  id <- generateId
  return $ Client id d

createDistance :: [Facility] -> [Client] -> (Int, Int, Double) -> Maybe Distance
createDistance fs cs (i, j, c) =
  Distance <$> (facilityId <$> findFacility fs i)
           <*> (clientId <$> findClient cs j)
           <*> Just c
           <*> Just 0.0

runIdManagement :: IdManagement a -> a
runIdManagement m = evalState m 0

createFacilitiesFromList :: [(Double, Double)] -> [Facility]
createFacilitiesFromList list = runIdManagement $ mapM createFacility list

createClientsFromList :: [Double] -> [Client]
createClientsFromList list = runIdManagement $ mapM createClient list

createDistanceFromList :: [Facility] -> [Client] -> [(Int, Int, Double)] -> Maybe [Distance]
createDistanceFromList fac clients = mapM (createDistance fac clients)

randomFacilities :: Int -> IO Facilities
randomFacilities n =
  do let range = (0.0, 100.0)
     g <- newStdGen
     h <- newStdGen
     let f = take n (randomRs range g)
     let u = take n (randomRs range h)
     return $ zipWith4 Facility [0,1..] f u [0.0, 0.0..]

randomClients :: Int -> IO Clients
randomClients m = do
  let range = (0.0, 100.0)
  g <- newStdGen
  let d = take m $ randomRs range g
  return $ zipWith Client [0..] d


randomDistances :: Int -> Int -> IO Distances
randomDistances n m =
  do let range = (0.0, 100.0)
     g <- newStdGen
     h <- newStdGen
     let f = zip [0,1..] $ take n $ uncurry zip $ splitAt n (randomRs range g)
     let c = zip [0,1..] $ take m $ uncurry zip $ splitAt m (randomRs range h)
     return $ array ((0,0),(n-1,m-1)) [ ((i, j), Distance i j (sqrt ((cx-fx)**2 + (cy-fy)**2)) 0.0) | (i, (fx,fy)) <- f
                                                                                                    , (j, (cx,cy)) <- c]

randomCFLP :: Int -> Int -> IO CFLP
randomCFLP n m =
  do fs <- randomFacilities n
     cs <- randomClients m
     ds <- randomDistances n m
     return $ CFLP fs cs ds


openFacility :: Facility -> Double -> Facility
openFacility f y = f { y = y }

--getSol :: VS.Vector a -> Facility -> a
getSol sol f = sol VS.! facilityId f

openFacilitiesCFLP :: CFLP -> CpxSolution -> CFLP
openFacilitiesCFLP cflp sol = cflp { facilities = openFacilities (facilities cflp) ys
                                   , distances = satisfyDemand cflp xs
                                   }
  where n = length $ facilities cflp
        ys = take n $ VS.toList (solX (traceMsgId "sol " sol))
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
                                  -> ((i, j), Distance i' j' c (xs !! (mcfXIdx n m (Map.findWithDefault 0 i fIds') j))))
                              (traceMsgId "filt: " $ ((filter (\((i, j), _) -> i `elem` fIds)) (assocs ds))))
  where ds = distances mcf
        fIds = traceMsgId "fids: " $ map facilityId $ facilities mcf
        fIds' = Map.fromList $ zip fIds [0..]
        n = length $ facilities mcf --inc . fst . snd $ bounds ds
        m = length $ clients mcf --inc . snd . snd $ bounds ds

  -- let fIds = map head (group . sort $ map (\(Distance i _ _ _) -> i) ds)
  --                         cIds = map head (group . sort $ map (\(Distance _ j _ _) -> j) ds)
  --                         findD i j = find (\(Distance s t _ _) -> i == s && j == t)
  --                     in zipWith satisfy (traceMsgId "cij: " [fromJust $ findD i j ds | j <- cIds, i <- fIds]) (traceMsgId "xij: " xs)

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
        fj = [i | (Facility i _ _ _) <- fs, let xi = head $ getXs (distances cflp) [i] [j], xi > 0.0]
        nk = concatMap (\(Cluster k nk) -> nk) cluster
        ks = map clusterCenter cluster
        ds = distances cflp
        bj = [i | i <- fj
                , i `notElem` nk
                , maybeToGuard $ isNearer <$> (getDistanceById ds i j) <*> sequence [getDistanceById ds i k | k <- ks]]

getXs :: Distances -> [FacilityId] -> [ClientId] -> [Double]
getXs ds is js = [ x $ ds!(i,j) | i <- is, j <- js]

-- map x $ filter (\(Distance i j _ _) -> i `elem` is && j `elem` js) ds


getPossibleCenters :: CFLP -> [Cluster] -> [ClientId]
getPossibleCenters cflp currentClusters = do
  let unclustered = (map clientId $ clients cflp) \\ map clusterCenter currentClusters
  j <- unclustered
  let bj = calculateBj currentClusters cflp j
      xs = getXs (distances cflp) bj [j]
  guard $ sum xs >= 0.5
  return j


-- While there are possible center, form cluster around the center
c1 :: CFLP -> CpxSolution -> [Cluster] -> [ClientId] -> [Cluster]
c1 cflp sol c [] = traceMsgId "c1 []: " c
c1 cflp sol c s  = (traceMsgId "c1   : " $ formCluster c cflp s (getBudget cflp sol)) : c


-- Step C2

facilityDistances :: CFLP -> FacilityId -> [Distance]
facilityDistances cflp i = [ ds!(i,j) | j <- [0..(length $ clients cflp) - 1] ]
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
  where openFacilities = [i | (Facility i _ _ yi) <- facilities cflp, yi > 0.0]
        clusteredFacilities = concatMap clusterElements clusters
        remainingFacilities = openFacilities \\ clusteredFacilities
        facilityAssignment = zip remainingFacilities $ map (nearestClient cflp) remainingFacilities


traceMsgId :: Show a => String -> a -> a
traceMsgId msg val = trace (msg ++ show val) val


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
showSnFacilities fs = "i: " ++ (unwords $ map (printf "%d") $ map snFacilityId fs) ++ "\n"
                      ++ showFormat "f: " snOpeningCost fs ++ "\n"
                      ++ showFormat "u: " snCapacity fs ++ "\n"
                      ++ showFormat "c: " snDistance fs ++ "\n"
                      ++ showFormat "x: " snDemand fs


clusterToSNCFLP :: CFLP -> Cluster -> Maybe (ClientId, SNCFLP)
clusterToSNCFLP cflp (Cluster k nk) = if null snfs then Nothing
                                      else Just (k, sncflp)
  where fs = catMaybes $ map (findFacility (facilities cflp)) nk
        cs = map clientId (clients cflp)
        ds = distances cflp
        lk = filter (\i -> y i < 1.0) fs

        snfids = map facilityId lk
        snocs  = map f lk
        sncs   = map u lk
        snds   = catMaybes $ map (\i -> getDistanceById ds i k) snfids

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
getOpenedFacilitiesFromClusters cflp clusters = filter (\i -> y i == 1.0) fs
  where nk = concatMap clusterElements clusters
        fs = catMaybes $ map (findFacility (facilities cflp)) nk

getOpenedFacilitiesFromSNCFLPs :: CFLP -> [SNCFLP] -> Facilities
getOpenedFacilitiesFromSNCFLPs cflp sncflps = fs
  where nk = filter (\f -> snDemand f == 1.0 ) $ concatMap snFacilities sncflps
        fs = catMaybes $ map (findFacility (facilities cflp)) $ map snFacilityId nk


-- Assign clients

sol :: CFLP -> IO ()
sol cflp = do
  lpSol <- solLp "CFLP" $ fromJust . fromCFLP $ cflp

  putStrLn $ "lp x      : " ++ show (solX lpSol)
  putStrLn $ "lp solstat: " ++ show (solStat lpSol)
  putStrLn $ "lp objval : " ++ show (solObj lpSol)

  let openedCFLP = openFacilitiesCFLP cflp lpSol

  putStrLn $ "Open Facilities from LP:"
  print $ map facilityId (filter (\f -> y f == 1.0) (facilities openedCFLP))

  let cflp = openedCFLP
  print "Possible Centers"
  print $ getPossibleCenters cflp []
  print $ chooseNextCenter (getPossibleCenters cflp [])
                           (getBudget cflp lpSol)
  print "Cluster:"
  let cs = c1 cflp lpSol [] (getPossibleCenters cflp [])
  printClusters cs
  let cs' = c2 cflp cs
  print "Updated Cluster:"
  printClusters cs'
  let sncflps = catMaybes $ map (clusterToSNCFLP cflp) cs'

  let sncflps' = map (solveSNCFLP . snd) $ sncflps

  let openedFs   = getOpenedFacilitiesFromClusters cflp cs'
      openedFs'  = getOpenedFacilitiesFromSNCFLPs cflp sncflps'
      openedFs'' = openedFs ++ openedFs'
      openedIds  = map facilityId openedFs''
  print "Open Facilities from Cluster:"
  print openedFs

  print "Open Facilities from SNCFLP:"
  print openedFs'

  print "Open IDs:"
  print $ map facilityId openedFs''

  print "Closed IDs:"
  print $ (map facilityId (facilities cflp)) \\ (map facilityId (openedFs ++ openedFs'))


  let openedCFLP = CFLP (filter (\f -> facilityId f `elem` openedIds) (facilities cflp))
                        (clients cflp)
                        (distances cflp)
--                        (array (bounds $ distances cflp) (map (\(_, d) -> i d `elem` openedIds) (assocs $ distances cflp)))


  let mcf = fromOpenedCFLP openedCFLP
  print $ obj mcf
  putStr $ showAmat (amat mcf)

--  print openedCFLP
--  print mcf
  mcfSol <- solLp "MCF" mcf

  print $ solX mcfSol

  let openedMcf = assignFacilitiesMCF openedCFLP mcfSol
  print openedMcf
  print "Total Cost: "
  print $ (solObj mcfSol)
  print $ sum (map f (facilities cflp))
  print $ (solObj mcfSol) + sum (map f (facilities cflp))
