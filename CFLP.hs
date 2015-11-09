{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Capacitated Facility Location Problem

module CFLP where

import Control.Monad
import qualified Control.Monad.State as State

import           Data.Array
import           Data.Function
import           Data.List                     (find, group, sort, sortBy, (\\))
import           Data.Tuple
import           Text.Printf

import qualified Data.Vector                   as V

import Control.Applicative hiding ((<|>))

import Text.ParserCombinators.Parsec
import Text.Parsec

import           CPLEX
import           MIP

type FacilityId = Int
data Facility = Facility { facilityId :: FacilityId
                         , f          :: Double -- opening cost
                         , u          :: Double -- capacity
                         , y          :: Double -- fraction opened
                         } deriving (Show, Eq)

type Facilities = [Facility]

type ClientId = Int
data Client = Client { clientId :: ClientId
                     , d        :: Double -- demand
                     } deriving (Show, Eq)

type Clients = [Client]

data Distance = Distance { i :: FacilityId
                         , j :: ClientId
                         , c :: Double -- cost
                         , x :: Double -- fraction satisfied
                         } deriving (Show, Eq)

--type Distances = [Distance]

-- data Distance = Distance { c :: Double -- cost
--                          , x :: Double -- fraction satisfied
--                          } deriving (Show)

type Distances = Array (FacilityId, ClientId) Distance

data CFLP = CFLP { facilities :: Facilities
                 , clients    :: Clients
                 , distances  :: Distances}
            deriving (Eq)



type IdManagement = State.State Int

generateId :: IdManagement Int
generateId = do
  n <- State.get
  State.put (n + 1)
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
runIdManagement m = State.evalState m 0

createFacilitiesFromList :: [(Double, Double)] -> [Facility]
createFacilitiesFromList list = runIdManagement $ mapM createFacility list

createClientsFromList :: [Double] -> [Client]
createClientsFromList list = runIdManagement $ mapM createClient list

createDistanceFromList :: [Facility] -> [Client] -> [(Int, Int, Double)] -> Maybe [Distance]
createDistanceFromList fac clients = mapM (createDistance fac clients)


showFormat prefix selector list = prefix ++ (unwords $ map (printf "%.2f") $ map selector list)

showFacilities fs = (unwords $ map (show . facilityId) fs) ++ "\n" ++
                    (unwords $ map (show . f) fs) ++ "\n" ++
                    (unwords $ map (show . u) fs) ++ "\n"

showFacilitiesSol fs = (unwords $ map (show . facilityId) fs) ++ "\n" ++
                       (unwords $ map (show . y) fs) ++ "\n"

showClients cs = (unwords $ map (show . clientId) cs) ++ "\n" ++
                 (unwords $ map (show . d) cs) ++ "\n"

showClientsSol cs = (unwords $ map (show . clientId) cs) ++ "\n"

showDistances ds = unlines [unwords $ line i | i <- [0..n]] ++ "\n"
  where (n,m) = snd . bounds $ ds
        line i = [show . c $ ds!(i,j) | j <- [0..m]]
showDistancesSol ds = unlines [unwords [show . x $ ds!(i,j) | j <- [0..m]] | i <- [0..n]] ++ "\n"
  where (n,m) = snd . bounds $ ds


showCFLP cflp = (show . length . facilities $ cflp) ++ "\n" ++
                (showFacilities . facilities $ cflp) ++ "\n" ++
                (show . length . clients $ cflp) ++ "\n" ++
                (showClients . clients $ cflp) ++ "\n" ++
                (showDistances . distances $ cflp) ++ "\n"

showOpenFacilites cflp = (show . length . facilities $ cflp) ++ "\n" ++
                         (showFacilitiesSol $ filter (\f -> y f > 0) (facilities cflp)) ++ "\n"

showCFLPSolution cflp = (show . length . facilities $ cflp) ++ "\n" ++
                        (showFacilitiesSol . facilities $ cflp) ++ "\n" ++
                        (show . length . clients $ cflp) ++ "\n" ++
                        (showClientsSol . clients $ cflp) ++ "\n" ++
                        (showDistancesSol . distances $ cflp) ++ "\n"

-- | ShowS implementation

showsWords :: (Show a) => [a] -> ShowS
showsWords [w]       s = shows w s
showsWords (w : ws)  s = shows w (foldr (\w' s -> ' ' : shows w' s) s ws)

showsLines :: (Show a) => [[a]] -> ShowS
showsLines [l]       s = showsWords l s
showsLines (l : ls)  s = showsWords l (foldr (\l' s -> '\n' : showsWords l' s) s ls)

showsWords' :: [ShowS] -> ShowS
showsWords' [w] s      = w s
showsWords' (w : ws) s = w (foldr (\w s' -> ' ' : w s') s ws)

showsLines' :: [ShowS] -> ShowS
showsLines' [l]      s = l s
showsLines' (l : ls) s = l (foldr (\l s' -> '\n' : l s') s ls)

showsFacilities fs s = showsLines' [showsWords' (map (shows . facilityId) fs),
                                    showsWords' (map (shows . f) fs),
                                    showsWords' (map (shows . u) fs)] ('\n' : s)

showsFacilitiesSol fs s = showsLines' [showsWords' (map (shows . facilityId) fs),
                                       showsWords' (map (shows . y) fs)] s

showsClients cs s = showsLines' [showsWords' (map (shows . clientId) cs),
                                 showsWords' (map (shows . d) cs)] ('\n' : s)

showsDistances ds s = showsLines' [showsWords' [shows . c $ ds!(i,j) | j <- [0..m]] | i <- [0..n]] ('\n' : s)
  where (n,m) = snd . bounds $ ds

showsDistancesSol ds s = showsLines' [showsWords' [shows . x $ ds!(i,j) | j <- [0..m]] | i <- [0..n]] ('\n' : s)
  where (n,m) = snd . bounds $ ds


showCFLP'' cflp = showsLines' [shows . length . facilities $ cflp,
                               showsFacilities . facilities $ cflp,
                               shows . length . clients $ cflp,
                               showsClients . clients $ cflp,
                               showsDistances . distances $ cflp] ""

-- | Adapted from https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec

number = many1 digit
plus = char '+' *> number
minus = char '-' <:> number

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

integer :: (Stream s m Char) => ParsecT s u m Integer
integer = rd <$> (pos <|> neg <|> number)
  where rd     = read :: String -> Integer
        pos    = char '+' *> number
        neg    = (:) <$> char '-' <*> number
        number = many1 digit

integerList :: (Stream s m Char) => ParsecT s u m [Integer]
integerList = (integer `sepBy` char ' ') <* newline

double = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Double
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer
          integer  = plus <|> minus <|> number

doubleList :: (Stream s m Char) => ParsecT s u m [Double]
doubleList = (double `sepBy` char ' ') <* newline

cflpFile :: (Stream s m Char) => ParsecT s u m CFLP
cflpFile = do
  -- number of facilities
  n <- fromIntegral <$> integer
  newline

  fIds <- integerList
  fs <- doubleList
  us <- doubleList
  newline

  -- number of clients
  m <- fromIntegral <$> integer
  newline

  cIds <- integerList
  ds <- doubleList
  newline

  -- distances
  cijs <- replicateM n doubleList

  let cijs' = concat $ zipWith (\i ci -> zipWith (\j cij -> ((i, j), Distance i j cij 0.0)) [0..] ci) [0..] cijs

  let facilities = createFacilitiesFromList $ zip fs us
      clients    = createClientsFromList ds
--      Just distances  = createDistanceFromList facilities clients cijs'
  return $ CFLP facilities clients (array ((0,0),(n-1,m-1)) cijs')

instance Show CFLP where
  show = showCFLP

isFeasible :: CFLP -> Bool
isFeasible (CFLP fs cs _) = sum (map u fs) >= sum ( map d cs)


-- | Accessors

findClient :: Clients -> Int -> Maybe Client
findClient cs j = find isClient cs
  where isClient (Client id _) = id == j

findFacility :: Facilities -> Int -> Maybe Facility
findFacility fs i = find isFacility fs
  where isFacility (Facility id _ _ _) = id == i

findDistance :: Distances -> Int -> Int -> Maybe Distance
findDistance ds i j = find isDistance ds
  where isDistance (Distance from to _ _) = i == from && j == to


getCapacityById :: Facilities -> Int -> Maybe Double
getCapacityById fs i = u <$> find (\f -> facilityId f == i) fs

getDemandById :: Clients -> Int -> Maybe Double
getDemandById cs j = d <$> find (\c -> clientId c == j) cs

getDistanceById :: Distances -> FacilityId -> ClientId -> Maybe Double
--getDistanceById ds i j = c <$> find (\(Distance s t _ _) -> i == s && j == t) ds
getDistanceById ds i j = Just . c $ ds!(i,j)




-- | CFLP -> LP

createObjFromCFLP :: CFLP -> [Double]
createObjFromCFLP (CFLP fac clients dists) =
  [f | (Facility _ f _ _) <- fac]

seqTuple :: (a, b, Maybe c) -> Maybe (a, b, c)
seqTuple (a, b, Just c) = Just (a, b, c)
seqTuple (_, _, Nothing) = Nothing

createObjIndexedListFromCFLP :: CFLP -> Maybe [(Int, Int, Double)]
createObjIndexedListFromCFLP cflp@(CFLP _ cs ds) =
  sequence [seqTuple (i, j, (*) <$> demandOf j <*> Just c) | (Distance i j c _) <- elems ds]
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


xIdx :: Int -> Int -> Int -> Int -> Int
xIdx n m i j  | (0 <= i) && (i < n) && (0 <= j) && (j < m) = n + (j*n + i)
xIdx n m i j  | otherwise = error $ show (i, j) ++ " out of bounds " ++ show (n, m)

yIdx :: Int -> Int -> Int -> Int
yIdx n m i    | (0 <= i) && (i < n)                        =            i

xIdx' :: Int -> Int -> Int -> (Int, Int)
xIdx' n m k | (n <= k) && (k < n*m) = swap $ divMod (k-n) n

yIdx' :: Int -> Int -> Int -> Int
yIdx' n m k | (0 <= k) && (k < n)   = k


ctr1 :: Int -> Int -> [[(Int, Double)]]
ctr1 n m = [ [(xIdx n m i j, 1.0) | i <- [0..n-1]] | j <- [0..m-1] ]

ctr2 :: Int -> Int -> [[(Int, Double)]]
ctr2 n m = [ [(yIdx n m i, -1.0), (xIdx n m i j, 1.0)] | i <- [0..n-1], j <- [0..m-1] ]

ctr3 :: Facilities -> Clients -> Int -> Int -> [[(Int, Double)]]
ctr3 fs cs n m = [ [(xIdx n m i j, dj)
                  | j <- [0..m-1], let Just dj = getDemandById cs j ]
                  ++ [(yIdx n m i, -ui)]
                | i <- [0..n-1], let Just ui = getCapacityById fs i]

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
