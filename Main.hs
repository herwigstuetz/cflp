{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.State
import           Control.Monad.Trans.Maybe
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

import           CFLP

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


data MIP = MIP { sense  :: ObjSense
               , obj    :: V.Vector Double
               , rhs    :: V.Vector Sense
               , amat   :: [(Row, Col, Double)]
               , bnd    :: V.Vector (Maybe Double, Maybe Double)
               , ctypes :: V.Vector Type
               } deriving (Show)

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
     return [ Distance i j (sqrt ((cx-fx)**2 + (cy-fy)**2)) 0.0 | (i, (fx,fy)) <- f
                                                                , (j, (cx,cy)) <- c]

randomCFLP :: Int -> Int -> IO CFLP
randomCFLP n m =
  do fs <- randomFacilities n
     cs <- randomClients m
     ds <- randomDistances n m
     return $ CFLP fs cs ds


createObjFromCFLP :: CFLP -> [Double]
createObjFromCFLP (CFLP fac clients dists) =
  [f | (Facility _ f _ _) <- fac]

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


xIdx n m i j  | (0 <= i) && (i < n) && (0 <= j) && (j < m) = n + (j*n + i)
yIdx n m i    | (0 <= i) && (i < n)                        =            i

ctr1 :: Int -> Int -> [[(Int, Double)]]
ctr1 n m = [ [(xIdx n m i j, 1.0) | i <- [0..n-1]] | j <- [0..m-1] ]

ctr2 :: Int -> Int -> [[(Int, Double)]]
ctr2 n m = [ [(yIdx n m i, -1.0), (xIdx n m i j, 1.0)] | i <- [0..n-1], j <- [0..m-1] ]

ctr3 :: Facilities -> Clients -> Int -> Int -> [[(Int, Double)]]
ctr3 fs cs n m = [ [(xIdx n m i j, dj)
                  | j <- [0..m-1], let Just dj = getDemandById cs j ]
                  ++ [(yIdx n m i, -ui)]
                | i <- [0..n-1], let Just ui = getCapacityById fs i]

seqTuple :: (a, b, Maybe c) -> Maybe (a, b, c)
seqTuple (a, b, Just c) = Just (a, b, c)
seqTuple (_, _, Nothing) = Nothing

fromConstraints :: [[(Int, Double)]] -> [(Row, Col, Double)]
fromConstraints l = map (\(r,c,x) -> (Row r, Col c, x)) (concatMap f $ zip [0..] l)
  where f (r, l) = map (\(c, x) -> (r, c, x)) l

constraints fs cs n m = do
  Just $ fromConstraints $ (ctr1 n m) ++ (ctr2 n m) ++ (ctr3 fs cs n m)

rhs1 n m = [G 1.0 | j <- [0..m-1]]
rhs2 n m = [L 0.0 | i <- [0..n-1], j <- [0..m-1]]
rhs3 n m = [L 0.0 | i <- [0..n-1]]

createRhs :: Int -> Int -> V.Vector Sense
createRhs n m = V.fromList $ rhs1 n m ++ rhs2 n m ++ rhs3 n m

ybnds :: Int -> Int -> [(Maybe Double, Maybe Double)]
ybnds n m = [(Just 0.0, Just 1.0) | i <- [0..n-1]]

xbnds :: Int -> Int -> [(Maybe Double, Maybe Double)]
xbnds n m = [(Just 0.0, Nothing) | i <- [0..n-1], j <- [0..m-1]]

bnds :: Int -> Int -> V.Vector (Maybe Double, Maybe Double)
bnds n m = V.fromList $ ybnds n m ++ xbnds n m

varTypes :: Int -> Int -> V.Vector Type
varTypes n m = V.fromList $ replicate (length $ ybnds n m) CPX_BINARY
               ++ replicate (length $ xbnds n m) CPX_CONTINUOUS

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

runLp :: MIP -> CpxEnv -> CpxLp -> IO (Maybe String)
runLp (MIP sense obj rhs amat bnd _) cpxEnv cpxLp =
  copyLp cpxEnv cpxLp sense obj rhs amat bnd

runMip :: MIP -> CpxEnv -> CpxLp -> IO (Maybe String)
runMip (MIP sense obj rhs amat bnd ctypes) cpxEnv cpxLp =
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
        fs = map head (group . sort $ map (\(Row i, _, _) -> i) amat)
        cs = map head (group . sort $ map (\(_, Col j, _) -> j) amat)
        n = length fs
        m = length cs

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
                      in zipWith satisfy (traceMsgId "cij: " [fromJust $ findD i j ds | j <- [0..m], i <- [0..n]]) (traceMsgId "xij: " xs)

assignFacilitiesMCF :: CFLP -> CpxSolution -> CFLP
assignFacilitiesMCF mcf sol = mcf { distances = satisfyDemand (distances mcf) (VS.toList (solX sol)) }

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
facilityDistances cflp i = filter (\(Distance r s _ _) -> i == r) (distances cflp)

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
traceMsgId msg val = val --trace (msg ++ show val) val


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

mcfXIdx n m i j | (0 <= i) && (i < n) && (0 <= j) && (j < m) =      j*n + i


-- ERROR: mixing of i=0..n-1 and i in facilityIds (!= 0..n-1)
mcfObj :: CFLP -> V.Vector Double
mcfObj cflp = V.fromList $ map snd $ sortBy (compare `on` fst) [ (mcfXIdx n m i j, dj * cij) | (i, fId) <- zip [0..n-1] fIds, (j, cId) <- zip [0..m-1] cIds, let Just cij = getDistanceById ds fId cId, let Just dj = getDemandById cs cId]
  where n = length $ facilities cflp
        m = length $ clients cflp
        cs = clients cflp
        ds = distances cflp
        fIds = map facilityId (facilities cflp)
        cIds = map clientId (clients cflp)

mcfRhs :: CFLP -> V.Vector Sense
mcfRhs cflp = V.fromList $
                replicate m (G 1.0) ++ -- \sum_i x_{ij} \geq 1 \forall j
                replicate (n * m) (L 1.0) ++ -- x_{ij} \leq 1 \forall i,j
                map (L . u) (facilities cflp) -- \sum_j d_j x_{ij} \leq u_i \forall i, this needs that ids are ordered
  where n = length (facilities cflp)
        m = length (clients cflp)

mcfConstraints :: CFLP -> [(Row, Col, Double)]
mcfConstraints cflp = fromConstraints $
                        [[(mcfXIdx n m i j, 1.0) | (i, fId) <- zip [0..] fIds] | (j, cId) <- zip [0..] cIds] ++
                        [[(mcfXIdx n m i j, 1.0)] | i <- [0..n-1], j <- [0..m-1]] ++
                        [[(mcfXIdx n m i j, dj) | (j, cId) <- zip [0..m-1] cIds, let Just dj = getDemandById cs cId] | i <- [0..n-1]]
  where cIds = map clientId (clients cflp)
        fIds = map facilityId (facilities cflp)
        cs = clients cflp
        n = length fIds
        m = length cIds

mcfBnds :: CFLP -> V.Vector (Maybe Double, Maybe Double)
mcfBnds cflp = V.fromList $ replicate (n * m) (Just 0.0, Nothing)
  where n = length (facilities cflp)
        m = length (clients cflp)

mcfTypes :: CFLP -> V.Vector Type
mcfTypes cflp = V.fromList $ replicate (n * m) CPX_CONTINUOUS
  where n = length (facilities cflp)
        m = length (clients cflp)

fromOpenedCFLP :: CFLP -> MIP
fromOpenedCFLP cflp = let s = CPX_MIN
                          o = mcfObj cflp
                          r = mcfRhs cflp
                          a = mcfConstraints cflp
                          b = mcfBnds cflp
                          t = mcfTypes cflp
                      in
                        MIP s o r a b t

solMip :: String -> MIP -> IO (CpxSolution)
solMip name p = withEnv $ \env -> do
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  withLp env name $ \mip -> do
    putStrLn $ "Solving " ++ name ++ ":"

    statusMip <- runMip p env mip

    case statusMip of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg
    putStrLn $ "Solving " ++ name ++ ":"

    -- Solve problem
    statusOpt <- mipopt env mip
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXlpopt error: " ++ msg
    putStrLn $ "Solving " ++ name ++ ":"

    -- Retrieve solution
    statusSol <- getMIPSolution env mip
    case statusSol of Left msg -> error msg
                      Right sol -> return sol

solLp :: String -> MIP -> IO (CpxSolution)
solLp name p = withEnv $ \env -> do
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  withLp env name $ \lp -> do
    putStrLn $ "Solving " ++ name ++ ":"

    statusLp <- runLp p env lp

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg
    putStrLn $ "Solving " ++ name ++ ":"

    -- Solve problem
    statusOpt <- lpopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXlpopt error: " ++ msg
    putStrLn $ "Solving " ++ name ++ ":"

    -- Retrieve solution
    statusSol <- getSolution env lp
    case statusSol of Left msg -> error msg
                      Right sol -> return sol

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
                        (filter (\d -> i d `elem` openedIds) (distances cflp))


  let mcf = fromOpenedCFLP openedCFLP
  print $ obj mcf
  putStr $ showAmat (amat mcf)

  print openedCFLP
  print mcf
  mcfSol <- solLp "MCF" mcf

  print $ solX mcfSol
  let openedMcf = assignFacilitiesMCF openedCFLP mcfSol
  print openedMcf
  print "Total Cost: "
  print $ (solObj mcfSol)
  print $ sum (map f (facilities cflp))
  print $ (solObj mcfSol) + sum (map f (facilities cflp))

cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0
