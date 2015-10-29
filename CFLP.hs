-- | Capacitated Facility Location Problem

module CFLP where

import           Data.Function
import           Data.List     (find, group, sort, sortBy, (\\))
import           Text.Printf

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

showFormat prefix selector list = prefix ++ (unwords $ map (printf "%.2f") $ map selector list)

showFacilityIds :: Facilities -> String
showFacilityIds fs = "  i: " ++ (unwords $ map (show . facilityId) fs)

showOpeningCosts :: Facilities -> String
showOpeningCosts fs = showFormat "f_i: " f fs

showCapacities   :: Facilities -> String
showCapacities   fs = showFormat "u_i: " u fs

showFractionOpen :: Facilities -> String
showFractionOpen fs = showFormat "y_i: " y fs

showFacilities fs = (showFacilityIds fs) ++ "\n" ++
                    (showOpeningCosts fs) ++ "\n" ++
                    (showCapacities fs) ++ "\n" ++
                    (showFractionOpen fs)

showClientIds :: Clients -> String
showClientIds cs = "  j: " ++ (unwords $ map (show . clientId) cs)

showDemands :: Clients -> String
showDemands cs = showFormat "d_j: " d cs

showClients cs = (showClientIds cs) ++ "\n" ++
                 (showDemands cs)

showCosts :: Distances -> String
showCosts ds = showFormat ("f_" ++ show (i (ds !! 0)) ++ ": ") c ds

showFlow :: Distances -> String
showFlow ds = showFormat ("f_" ++ show (i (ds !! 0)) ++ ": ") x ds

distancesFromFacility ds i' = sortBy (compare `on` j) $ [distance | distance <- ds, i distance == i']
distancesToClient ds j' = sortBy (compare `on` i) $ [distance | distance <- ds, j distance == j']

showDistancesElement :: (Distances -> String) -> Distances -> String
showDistancesElement selector ds = ("    " ++ (unwords $ map (printf "  c_%d") cs))
                   ++ "\n" ++
                   (unlines $ map selector [distancesFromFacility ds i | i <- fs])
  where
    fs = map head (group . sort $ map i ds)
    cs = map head (group . sort $ map j ds)

showDistances ds = (showDistancesElement showCosts ds)
                   ++ "\n" ++
                   (showDistancesElement showFlow ds)

showCFLP cflp = (showFacilities $ facilities cflp) ++ "\n\n"
                ++ (showClients $ clients cflp) ++ "\n\n"
                ++ (showDistances $ distances cflp)

instance Show CFLP where
  show = showCFLP
