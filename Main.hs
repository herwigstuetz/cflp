{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.List (find, sortBy, intercalate)
import Data.Maybe
import Control.Monad.State
import Control.Monad.Trans.Maybe

import GHC.IO.Handle
import System.IO
import System.Directory

import Foreign.C

import CPLEX.Param
import CPLEX


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


data Facility = Facility { facilityId :: Int
                         , f :: Double -- opening cost
                         , u :: Double -- capacity
                         , y :: Double -- fraction opened
                         } deriving (Show)

type Facilities = [Facility]

data Client = Client { clientId :: Int
                     , d :: Double -- demand
                     } deriving (Show)

type Clients = [Client]

data Distance = Distance { i :: Facility
                         , j :: Client
                         , c :: Double -- cost
                         , x :: Double -- fraction satisfied
                         } deriving (Show)

type Distances = [Distance]

data CFLP = CFLP { fs :: Facilities
                 , cs :: Clients
                 , ds :: Distances}
            deriving (Show)


data LP = LP { sense :: ObjSense
             , obj :: V.Vector Double
             , rhs :: V.Vector Sense
             , amat :: [(Row, Col, Double)]
             , bnd :: V.Vector (Maybe Double, Maybe Double)
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
createDistance facilities clients (i, j, c) =
    Distance
      <$> find isFac facilities
      <*> find isClient clients
      <*> Just c
      <*> Just 0.0
      where isFac (Facility id _ _ _) = id == i
            isClient (Client id _) = id == j

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

createObjIndexedListFromCFLP :: CFLP -> [(Int, Int, Double)]
createObjIndexedListFromCFLP (CFLP _ _ ds) =
  [(i, j, d * c) | (Distance (Facility i _ _ _) (Client j d) c _) <- ds]

sortObjList :: [(Int, Int, a)] -> [(Int, Int, a)]
sortObjList = sortBy f
  where f (i1, j1, _) (i2, j2, _) | j1 == j2 && i1 == i2 = EQ
                                  | j1 < j2 = LT
                                  | j1 == j2 && i1 < i2 = LT
                                  | otherwise = GT

createObj :: CFLP -> V.Vector Double
createObj p@(CFLP fs cs ds) = V.fromList (createObjFromCFLP p ++ map f (sortObjList $ createObjIndexedListFromCFLP p))
  where f (_,_,d) = d

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

seqTuple :: (Row, Col, Maybe Double) -> Maybe (Row, Col, Double)
seqTuple (Row r, Col s, Just d) = Just (Row r, Col s, d)
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
  [(i,j) | (Distance (Facility i _ _ _) (Client j _) _ _) <- dists]


fromCFLP :: CFLP -> Maybe LP
fromCFLP cflp = let s = CPX_MIN
                    o = createObj cflp
                    n = length $ fs cflp
                    m = length $ cs cflp
                    r = createRhs n m
                    a = constraints (fs cflp) (cs cflp) n m
                    b = bnds n m
                in
                  LP s o r <$> a <*> Just b
runLP :: LP -> CpxEnv -> CpxLp -> IO (Maybe String)
runLP (LP sense obj rhs amat bnd) cpxEnv cpxLp = copyLp cpxEnv cpxLp sense obj rhs amat bnd

showObjSense :: ObjSense -> String
showObjSense CPX_MAX = "max"
showObjSense CPX_MIN = "min"

showObj :: V.Vector Double -> String
showObj = show

showAMat :: [(Row, Col, Double)] -> V.Vector Sense -> String
showAMat amat rhs = intercalate "\n" [intercalate " " ([show $ a i j | j <- [0..n+n*m-1]] ++ [show $ rhs V.! i]) | i <- [0..n+n*m+m-1]]
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
openFacilitiesCFLP cflp sol = cflp { fs = openFacilities (fs cflp) ys
                                   , ds = satisfyDemand (ds cflp) xs
                                   }
  where n = length $ fs cflp
        ys = take n $ VS.toList (solX sol)
        xs = drop n $ VS.toList (solX sol)

openFacilities :: Facilities -> [Double] -> Facilities
openFacilities = zipWith openFacility

satisfy :: Distance -> Double -> Distance
satisfy d x = d { x = x }

satisfyDemand :: Distances -> [Double] -> Distances
satisfyDemand = zipWith satisfy

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
                        print $ openFacilitiesCFLP <$> testCFLP <*> Just sol
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
