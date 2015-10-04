{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Function
import           Data.List                 (find, intercalate, minimumBy, sort,
                                            sortBy, (\\))
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import qualified Data.Set                  as Set
import qualified Data.Vector               as V
import qualified Data.Vector.Storable      as VS
import           GHC.IO.Handle
import           System.Directory
import           System.IO

import           Foreign.C

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
  sol >>= print
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
            deriving (Show)


data LP = LP { sense :: ObjSense
             , obj   :: V.Vector Double
             , rhs   :: V.Vector Sense
             , amat  :: [(Row, Col, Double)]
             , bnd   :: V.Vector (Maybe Double, Maybe Double)
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

bnds n m = V.fromList $ ybnds n m ++ xbnds n m

objsen = CPX_MAX

test11 :: CFLP -> [(Int, Int)]
test11 (CFLP fac clients dists) =
  [(i,j) | (Distance i j _ _) <- dists]


fromCFLP :: CFLP -> Maybe LP
fromCFLP cflp = let s = CPX_MIN
                    o = createObj cflp
                    n = length $ facilities cflp
                    m = length $ clients cflp
                    r = createRhs n m
                    a = constraints (facilities cflp) (clients cflp) n m
                    b = bnds n m
                in
                  LP s <$> o <*> Just r <*> a <*> Just b
runLP :: LP -> CpxEnv -> CpxLp -> IO (Maybe String)
runLP (LP sense obj rhs amat bnd) cpxEnv cpxLp = copyLp cpxEnv cpxLp sense obj rhs amat bnd

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

showLP (LP sense obj rhs amat bnd) = showObjSense sense
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
                       } deriving (Show)

-- Step C1
initialClusters :: [Cluster]
initialClusters = []

initialPossibleCenters :: CFLP -> [ClientId]
initialPossibleCenters = map clientId . clients

chooseNextCenter :: [ClientId] -> [Double] -> ClientId
chooseNextCenter possibleCenters budget = snd $ minimum $ filter (\a -> snd a `elem` possibleCenters) $ zip budget [0..]

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


-- Assign remaining facilities to closest cluster center
c2 :: CFLP -> [Cluster] -> [Cluster]
c2 cflp clusters = undefined
  where openFacilities = filter (((>) 0) . y) facilities cflp --[i | (Facility i _ _ yi) <- facilities cflp, yi > 0.0]
        clusteredFacilities = concatMap clusterElements clusters
        remainingFacilities = openFacilities \\ clusteredFacilities
        facilityAssignment = zip remainingFacilities $ map (nearestClient cflp) remainingFacilities


traceMsgId :: Show a => String -> a -> a
traceMsgId msg val = trace (msg ++ show val) val


-- Reducing to single node

-- Assign clients



sol :: IO ()
sol = withEnv $ \env -> do
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  withLp env "CFLP" $ \lp -> do

    let cflp = testCFLP
        p = cflp >>= fromCFLP

    statusLp <- case p of
      Nothing -> return $ Just "No valid problem"
      Just p -> runLP p env lp

    print testCFLP
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
                        let openedCFLP = openFacilitiesCFLP <$> testCFLP <*> Just sol

                        case openedCFLP of
                          Nothing -> do
                            print "noopenedcflp"
                            return ()
                          Just cflp -> do
                            print "Possible Centers"
                            print $ getPossibleCenters cflp []


                        putStrLn $ "x      : " ++ show (solX sol)
                        putStrLn $ "pi'    : " ++ show (solPi sol)
                        putStrLn $ "slack  : " ++ show (solSlack sol)
                        putStrLn $ "dj     : " ++ show (solDj sol)
                        putStrLn $ "solstat: " ++ show (solStat sol)
                        putStrLn $ "objval : " ++ show (solObj sol)






cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0


sol' :: IO ()
sol' = withEnv $ \env -> do
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  withLp env "testprob" $ \lp -> do
    let objsen = CPX_MAX
        obj = V.fromList [1,2,3]
        rhs = V.fromList [L 20, L 30]
        xbnds = [(Just 0, Just 40), (Just 0, Nothing), (Just 0, Nothing)]
        amat = [ (Row 0, Col 0, -1)
               , (Row 1, Col 0, 1)
               , (Row 0, Col 1, 1)
               , (Row 1, Col 1, -3)
               , (Row 0, Col 2, 1)
               , (Row 1, Col 2, 1)
               ]
    statusLp <- copyLp env lp objsen obj rhs amat (V.fromList xbnds)

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    ------------------------
    let qmat = [ (Row 0, Col 0, -33)
               , (Row 1, Col 0, 6)
               , (Row 0, Col 1, 6)
               , (Row 1, Col 1, -22)
               , (Row 2, Col 1, 11.5)
               , (Row 1, Col 2, 11.5)
               , (Row 2, Col 2, -11)
               ]
    statusQuad <- copyQuad env lp qmat
    case statusQuad of
      Nothing -> return ()
      Just msg -> error $ "CPXcopyquad error: " ++ msg

    ------------------------
    statusOpt <- qpopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXqpopt error: " ++ msg

    statusSol <- getSolution env lp
    case statusSol of
      Left msg -> error $ "CPXsolution error: " ++ msg
      Right sol -> do
        putStrLn $ "x      : " ++ show (solX sol)
        putStrLn $ "pi'    : " ++ show (solPi sol)
        putStrLn $ "slack  : " ++ show (solSlack sol)
        putStrLn $ "dj     : " ++ show (solDj sol)
        putStrLn $ "solstat: " ++ show (solStat sol)
        putStrLn $ "objval : " ++ show (solObj sol)
