{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Function
import           Data.List                 (find, intercalate, maximumBy,
                                            minimumBy, sort, sortBy, splitAt,
                                            zipWith4, zipWith5, (\\))
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import qualified Data.Set                  as Set
import qualified Data.Vector               as V
import qualified Data.Vector.Storable      as VS
import           GHC.IO.Handle
import           System.Directory
import           System.IO
import           Text.Printf

import           Foreign.C
import           System.Random

import           Debug.Trace

import           CPLEX
import           CPLEX.Param


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
    else do solMip cflp-- >>= print
            sol cflp-- >>= print
--  sol' >>= print
--  print testFac
--  print testClient
--  print testDist
--  print $ createObjFromCFLP <$> testCFLP

type FacilityId = Int
data Facility = Facility { facilityId :: FacilityId
                         , f          :: Double -- opening cost
                         , u          :: Double -- capacity
                         , y          :: Double -- fraction opened
                         } deriving (Show)

type Facilities = [Facility]

type ClientId = Int
data Client = Client { clientId :: ClientId
                     , d        :: Double -- demand
                     } deriving (Show)

type Clients = [Client]

data Distance = Distance { i :: FacilityId
                         , j :: ClientId
                         , c :: Double -- cost
                         , x :: Double -- fraction satisfied
                         } deriving (Show)

type Distances = [Distance]

data CFLP = CFLP { facilities :: Facilities
                 , clients    :: Clients
                 , distances  :: Distances}


data MIP = MIP { sense  :: ObjSense
               , obj    :: V.Vector Double
               , rhs    :: V.Vector Sense
               , amat   :: [(Row, Col, Double)]
               , bnd    :: V.Vector (Maybe Double, Maybe Double)
               , ctypes :: V.Vector Type
               } deriving (Show)

showFormat prefix selector list = prefix ++ (unwords $ map (printf "%.2f") $ map selector list)

showOpeningCosts :: Facilities -> String
showOpeningCosts fs = showFormat "f_i: " f fs

showCapacities   :: Facilities -> String
showCapacities   fs = showFormat "u_i: " u fs

showFractionOpen :: Facilities -> String
showFractionOpen fs = showFormat "y_i: " y fs

showFacilities fs = (showOpeningCosts fs) ++ "\n" ++
                    (showCapacities fs) ++ "\n" ++
                    (showFractionOpen fs)

showDemands :: Clients -> String
showDemands cs = showFormat "d_j: " d cs

showClients cs = showDemands cs

showCosts :: Distances -> String
showCosts ds = showFormat ("f_" ++ show (i (ds !! 0)) ++ ": ") c ds

showFlow :: Distances -> String
showFlow ds = showFormat ("f_" ++ show (i (ds !! 0)) ++ ": ") x ds

distancesFromFacility ds i' = sortBy (compare `on` j) $ [distance | distance <- ds, i distance == i']
distancesToClient ds j' = sortBy (compare `on` i) $ [distance | distance <- ds, j distance == j']

showDistancesElement :: (Distances -> String) -> Distances -> String
showDistancesElement selector ds = ("    " ++ (unwords $ map (printf "  c_%d") $ take m ([0,1..] :: [Int])))
                   ++ "\n" ++
                   (unlines $ map selector [distancesFromFacility ds i | i <- [0..n-1]])
  where
    n = (maximum $ map i ds) + 1
    m = (maximum $ map j ds) + 1

showDistances ds = (showDistancesElement showCosts ds)
                   ++ "\n" ++
                   (showDistancesElement showFlow ds)

showCFLP cflp = (showFacilities $ facilities cflp) ++ "\n\n"
                ++ (showClients $ clients cflp) ++ "\n\n"
                ++ (showDistances $ distances cflp)

instance Show CFLP where
  show = showCFLP

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

isFeasible :: CFLP -> Bool
isFeasible (CFLP fs cs _) = (sum $ map u fs) >= (sum $ map d cs)

testFac =
  createFacilitiesFromList [(1,8), (2,3)]

testClient =
  createClientsFromList [1, 3, 7]

testDist =
  createDistanceFromList testFac testClient [(0,0,1), (0,1,1.2), (0,2,1.3),
                                             (1,0,2.1), (1,1,2.2), (1,2,2.3)]

n = length testFac
m = length testClient


testCFLP :: Maybe CFLP
testCFLP =
  case testDist of Nothing -> Nothing
                   Just dists -> Just $ CFLP testFac testClient dists


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
     return [ Distance i j (sqrt ((cx-fx)**2 + (cy-fy)**2)) 0.0 | (i, (fx,fy)) <- f
                                                                , (j, (cx,cy)) <- c]

randomCFLP :: Int -> Int -> IO CFLP
randomCFLP n m =
  do fs <- randomFacilities n
     cs <- randomClients m
     ds <- randomDistances n m
     return $ CFLP fs cs ds


-- copyLp ::
--   CpxEnv
--   -> CpxLp
--   -> ObjSense
--   -> V.Vector Double
--   -> V.Vector Sense
--   -> [(Row, Col, Double)]
--   -> V.Vector (Maybe Double, Maybe Double)
--   -> IO (Maybe String)
--   	-- Defined in ‘CPLEX’

-- fromCFLP :: CFLP -> (ObjSense, V.Vector Double)
-- fromCFLP (CFLP fac clients dists) =
--   let objsen = CPX_MIN
--       obj = create

createObjFromCFLP :: CFLP -> [Double]
createObjFromCFLP (CFLP fac clients dists) =
  [f | (Facility _ f _ _) <- fac]

--               ++

findClient :: Clients -> Int -> Maybe Client
findClient cs j = find isClient cs
  where isClient (Client id _) = id == j

findFacility :: Facilities -> Int -> Maybe Facility
findFacility fs i = find isFacility fs
  where isFacility (Facility id _ _ _) = id == i

findDistance :: Distances -> Int -> Int -> Maybe Distance
findDistance ds i j = find isDistance ds
  where isDistance (Distance from to _ _) = i == from && j == to

createObjIndexedListFromCFLP :: CFLP -> Maybe [(Int, Int, Double)]
createObjIndexedListFromCFLP cflp@(CFLP _ cs ds) =
  sequence [seqTuple (i, j, (*) <$> demandOf j <*> Just c) | (Distance i j c _) <- ds]
  where demandOf :: Int -> Maybe Double
        demandOf j = d <$> findClient cs j

sortObjList :: [(Int, Int, a)] -> [(Int, Int, a)]
sortObjList = sortBy f
  where f (i1, j1, _) (i2, j2, _) | j1 == j2 && i1 == i2 = EQ
                                  | j1 < j2 = LT
                                  | j1 == j2 && i1 < i2 = LT
                                  | otherwise = GT

createObj :: CFLP -> Maybe (V.Vector Double)
createObj p@(CFLP fs cs ds) = do
  let f (_,_,d) = d
  ys <- return $ createObjFromCFLP p
  xs <- createObjIndexedListFromCFLP p
  return $ V.fromList $ ys ++ map f (sortObjList xs)

-- | indices starting with 0
yCol :: Int -> Int -> Int -> Int
yCol _ _ = id

-- | indices starting with 0
xCol :: Int -> Int -> (Int, Int) -> Int
xCol n m (i, j) = j * n + i


getCapacityById :: [Facility] -> Int -> Maybe Double
getCapacityById fs i = u <$> find (\f -> facilityId f == i) fs

getDemandById :: [Client] -> Int -> Maybe Double
getDemandById cs j = d <$> find (\c -> clientId c == j) cs

shiftCol :: Col -> (Row, Col, Double) -> (Row, Col, Double)
shiftCol (Col n) (Row r, Col s, x) = (Row r, Col $ n + s, x)

shiftRow :: Row -> (Row, Col, Double) -> (Row, Col, Double)
shiftRow (Row n) (Row r, Col s, x) = (Row $ n + r, Col s, x)

constraints1y :: Int -> Int -> [(Row, Col, Double)]
constraints1y n m = []
constraints1x :: Int -> Int -> [(Row, Col, Double)]
constraints1x n m = concat [[(Row j, Col $ xCol n m (i, j), 1) | i <- [0..n-1]] | j <- [0..m-1]]

constraints2y n m = concat [[(Row $ xCol n m (i, j), Col j, -1) | i <- [0..n-1]] | j <- [0..m-1]]
constraints2x n m = [(Row i, Col i, 1) | i <- [0..n*m-1]]

maybeTuple :: (a, b, Maybe c) -> Maybe (a, b, c)
maybeTuple (a, b, Just c) = Just (a, b, c)
maybeTuple (a, b, Nothing) = Nothing

seqTuple :: (a, b, Maybe c) -> Maybe (a, b, c)
seqTuple (a, b, Just c) = Just (a, b, c)
seqTuple (_, _, Nothing) = Nothing

constraints3y :: [Facility] -> [Client] -> Int -> Int -> Maybe [(Row, Col, Double)]
constraints3y fs cs n m = sequence [seqTuple (Row i, Col i, (*) <$> Just (-1) <*> getCapacityById fs i) | i <- [0..n-1]]

constraints3x :: [Facility] -> [Client] -> Int -> Int -> Maybe [(Row, Col, Double)]
constraints3x fs cs n m = sequence $ concat [[seqTuple (Row i, Col $ xCol n m (i, j), getDemandById cs j) | j <- [0..m-1]] | i <- [0..n-1]]

constraints1 n m = constraints1y n m ++ map (shiftCol $ Col n) (constraints1x n m)
constraints2 n m = constraints2y n m ++ map (shiftCol $ Col n) (constraints2x n m)

constraints3 :: [Facility] -> [Client] -> Int -> Int -> Maybe [(Row, Col, Double)]
constraints3 fs cs n m = do
  ys <- constraints3y fs cs n m
  xs <- constraints3x fs cs n m
  return $ ys ++ map (shiftCol (Col n)) xs

constraints fs cs n m = do
  c1 <- Just $ constraints1 n m
  c2 <- Just $ constraints2 n m
  c3 <- constraints3 fs cs n m
  return $ c1 ++ map (shiftRow $ Row m) c2 ++ map (shiftRow $ Row (m + n*m)) c3

rhs1 n m = [G 1.0 | j <- [0..m-1]]
rhs2 n m = [L 0.0 | i <- [0..n-1], j <- [0..m-1]]
rhs3 n m = [L 0.0 | i <- [0..n-1]]

createRhs n m = V.fromList $ rhs1 n m ++ rhs2 n m ++ rhs3 n m

ybnds n m = [(Just 0.0, Just 1.0) | i <- [0..n-1]]
xbnds n m = [(Just 0.0, Nothing) | i <- [0..n-1], j <- [0..m-1]]

bnds :: Int -> Int -> V.Vector (Maybe Double, Maybe Double)
bnds n m = V.fromList $ ybnds n m ++ xbnds n m

varTypes :: Int -> Int -> V.Vector Type
varTypes n m = V.fromList $ take (length $ ybnds n m) (repeat CPX_BINARY)
               ++ take (length $ xbnds n m) (repeat CPX_CONTINUOUS)
objsen = CPX_MAX

test11 :: CFLP -> [(Int, Int)]
test11 (CFLP fac clients dists) =
  [(i,j) | (Distance i j _ _) <- dists]


fromCFLP :: CFLP -> Maybe MIP
fromCFLP cflp = let s = CPX_MIN
                    o = createObj cflp
                    n = length $ facilities cflp
                    m = length $ clients cflp
                    r = createRhs n m
                    a = constraints (facilities cflp) (clients cflp) n m
                    b = bnds n m
                    t = varTypes n m
                in
                  MIP s <$> o <*> Just r <*> a <*> Just b <*> Just t

runLP :: MIP -> CpxEnv -> CpxLp -> IO (Maybe String)
runLP (MIP sense obj rhs amat bnd _) cpxEnv cpxLp = copyLp cpxEnv cpxLp sense obj rhs amat bnd

runMIP :: MIP -> CpxEnv -> CpxLp -> IO (Maybe String)
runMIP (MIP sense obj rhs amat bnd ctypes) cpxEnv cpxLp = do
  copyMip cpxEnv cpxLp sense obj rhs amat bnd ctypes


showObjSense :: ObjSense -> String
showObjSense CPX_MAX = "max"
showObjSense CPX_MIN = "min"

showObj :: V.Vector Double -> String
showObj = show

showAMat :: [(Row, Col, Double)] -> V.Vector Sense -> String
showAMat amat rhs = intercalate "\n" [unwords ([show $ a i j | j <- [0..n+n*m-1]] ++ [show $ rhs V.! i]) | i <- [0..n+n*m+m-1]]
  where el (_, _, d) = d
        a :: Int -> Int -> Double
        a i j = fromMaybe 0.0 $ el <$> find (\(Row k, Col l, _) -> i == k && j == l) amat

showLP (MIP sense obj rhs amat bnd _) = showObjSense sense
                                        ++ showObj obj
                                        ++ "\n"
                                        ++ showAMat amat rhs

openFacility :: Facility -> Double -> Facility
openFacility f y = f { y = y }

--getSol :: VS.Vector a -> Facility -> a
getSol sol f = sol VS.! facilityId f

openFacilitiesCFLP :: CFLP -> CpxSolution -> CFLP
openFacilitiesCFLP cflp sol = cflp { facilities = openFacilities (facilities cflp) ys
                                   , distances = satisfyDemand (distances cflp) xs
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

satisfyDemand :: Distances -> [Double] -> Distances
satisfyDemand ds xs = let n = maximum $ map i ds
                          m = maximum $ map j ds
                          findD i j = find (\(Distance s t _ _) -> i == s && j == t)
                      in zipWith satisfy [fromJust $ findD i j ds | j <- [0..m], i <- [0..n]] xs



-- Clustering

--data Center = Center Int [Int] -- clientId [facilityId]
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

getDistanceById :: Distances -> FacilityId -> ClientId -> Maybe Double
getDistanceById ds i j = c <$> find (\(Distance s t _ _) -> i == s && j == t) ds

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
        nk = concatMap (\ (Cluster k nk) -> nk) cluster
        ks = map clusterCenter cluster
        ds = distances cflp
        bj = [i | i <- fj
                , i `notElem` nk
                , maybeToGuard $ isNearer <$> (getDistanceById ds i j) <*> sequence [getDistanceById ds i k | k <- ks]]

getXs :: Distances -> [FacilityId] -> [ClientId] -> [Double]
getXs ds is js = map x $ filter (\(Distance i j _ _) -> i `elem` is && j `elem` js) ds


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

facilityDistances :: CFLP -> FacilityId -> Distances
facilityDistances cflp i = filter (\ (Distance r s _ _) -> i == r) (distances cflp)

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
        lk = filter (\ i -> y i < 1.0) fs

        snfids = map facilityId lk
        snocs  = map f lk
        sncs   = map u lk
        snds   = catMaybes $ map (\ i -> getDistanceById ds i k) snfids

        totalDemand = sum $ getXs ds snfids cs
        snfs        = zipWith5 SNFacility snfids snocs sncs snds [0.0,0.0..]

        sncflp = SNCFLP snfs totalDemand

updateSNFacility :: SNFacility -> Double -> SNFacility
updateSNFacility f x = f { snDemand = x }

updateSNCFLP :: SNCFLP -> [Double] -> SNCFLP
updateSNCFLP (SNCFLP fs d) vs = SNCFLP (zipWith updateSNFacility fs vs) d

-- TODO: Test if the right values are used (vs = zipWith (/) ... us) vs. (vs = zipWith (/) ... ws) etc.
solveSNCFLP :: SNCFLP -> SNCFLP
solveSNCFLP sncflp = updateSNCFLP sncflp vs'
--  where (fs, us, cs) = unzip3 $ map (\ f -> (snOpeningCost f, snCapacity f, snDistance f)) $ snFacilities sncflp
  where fs = map snOpeningCost $ snFacilities sncflp
        us = map snCapacity $ snFacilities sncflp
        cs = map snDistance $ snFacilities sncflp
        ws = zipWith3 (\ f u c -> f/u + c) fs us cs
        (order, ws') = unzip $ sortBy (compare `on` snd) $ zip [1..] ws
        vs = zipWith (/) (greedySolve ws $ snTotalDemand sncflp) us
        (order', vs') = unzip $ sortBy (compare `on` fst) $ zip order vs

greedySolve :: [Double] -> Double -> [Double]
greedySolve []       d = []
greedySolve vs       0 = replicate (length vs) 0.0
greedySolve (v : vs) d | d < v  = d : greedySolve vs 0.0
                       | d >= v = v : greedySolve vs (d - v)

-- Assign clients



solMip :: CFLP -> IO ()
solMip cflp = withEnv $ \env -> do
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  withLp env "CFLP" $ \lp -> do

--    cflp <- randomCFLP 10 10
    let p = fromCFLP cflp
--    let cflp = testCFLP
--    let p = cflp >>= fromCFLP

    print cflp
--    print p

    statusLp <- case p of
      Nothing -> return $ Just "No valid problem"
      Just p -> runMIP p env lp

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    -- Solve problem

    statusOpt <- mipopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXlpopt error: " ++ msg


    -- Retrieve solution
    statusSol <- getMIPSolution env lp
    case statusSol of Left msg -> error msg
                      Right sol -> do
                        let openedCFLP = openFacilitiesCFLP cflp sol
                        print openedCFLP
                        putStrLn $ "mip x      : " ++ show (solX sol)
--                        putStrLn $ "mip pi'    : " ++ show (solPi sol)
--                        putStrLn $ "mip slack  : " ++ show (solSlack sol)
--                        putStrLn $ "mip dj     : " ++ show (solDj sol)
                        putStrLn $ "mip solstat: " ++ show (solStat sol)
                        putStrLn $ "mip objval : " ++ show (solObj sol)


sol :: CFLP -> IO ()
sol cflp = withEnv $ \env -> do
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  withLp env "CFLP" $ \lp -> do

    let p = fromCFLP cflp
--    let cflp = testCFLP
--    let p = cflp >>= fromCFLP

    print cflp
--    print p

    statusLp <- case p of
      Nothing -> return $ Just "No valid problem"
      Just p -> runLP p env lp

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    -- Solve problem
    statusOpt <- lpopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXlpopt error: " ++ msg


    -- Retrieve solution
    statusSol <- getSolution env lp
    case statusSol of Left msg -> error msg
                      Right sol -> do

                        putStrLn $ "lp x      : " ++ show (solX sol)
--                        putStrLn $ "lp pi'    : " ++ show (solPi sol)
--                        putStrLn $ "lp slack  : " ++ show (solSlack sol)
--                        putStrLn $ "lp dj     : " ++ show (solDj sol)
                        putStrLn $ "lp solstat: " ++ show (solStat sol)
                        putStrLn $ "lp objval : " ++ show (solObj sol)

                        let openedCFLP = openFacilitiesCFLP cflp sol

--                        case openedCFLP of
--                          Nothing -> do
--                            print "noopenedcflp"
--                            return ()
--                          Just cflp -> do
                        let cflp = openedCFLP
                        print "Possible Centers"
                        print $ getPossibleCenters cflp []
                        print $ chooseNextCenter (getPossibleCenters cflp [])
                                                     (getBudget cflp sol)
                        print "Cluster:"
                        let cs = c1 cflp sol [] (getPossibleCenters cflp [])
                        printClusters cs
                        let cs' = c2 cflp cs
                        print "Updated Cluster:"
                        printClusters cs'
                        let sncflps = catMaybes $ map (clusterToSNCFLP cflp) cs'
                        print $ sncflps
                        print "Solved SNCFLPs"
                        print $ map (solveSNCFLP . snd) sncflps






cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0
