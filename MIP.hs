-- | Mixed Integer Program

module MIP where

import           Data.List   (find, group, intercalate, sort)
import           Data.Maybe  (fromMaybe)
import qualified Data.Vector as V

import           CPLEX
import           CPLEX.Param

data MIP = MIP { sense  :: ObjSense
               , obj    :: V.Vector Double
               , rhs    :: V.Vector Sense
               , amat   :: [(Row, Col, Double)]
               , bnd    :: V.Vector (Maybe Double, Maybe Double)
               , ctypes :: V.Vector Type
               } deriving (Show)

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
